#!/usr/bin/env python3
# -----------------------------------------------------------------------------
#  (C) Crown copyright 2026 Met Office. All rights reserved.
#  The file LICENCE, distributed with this code, contains details of the terms
#  under which the code may be used.
# -----------------------------------------------------------------------------
"""
Batch PSyclone driver for the LFRic build system.

Running the ``psyclone`` command once per algorithm file means paying the cost
of starting a Python interpreter and importing PSyclone, fparser and sympy
every time - between seven and ten seconds, and some sixteen thousand
filesystem metadata operations, on a typical site install where the libraries
live on shared NFS. A build with 800 algorithm files spends hours doing nothing
else, and concurrent builds multiply the load on the same NFS server.

This driver removes that cost from the *bulk* of the work. ``psyclone_psykal.mk``
calls it once per component, before its per-file rules run. It

* works out which algorithm files are out of date, using the same rules make
  would apply;
* imports PSyclone exactly once;
* forks a child per file, running several at a time.

Each child inherits the fully-imported interpreter through copy-on-write, so it
starts in milliseconds. Forking also means every file is transformed in a
pristine process: PSyclone keeps a great deal of global state (``Config``,
``LFRicConstants``, ``LFRicTypes``, ``ModuleManager``, fparser's symbol tables
and more) which would otherwise have to be reset by hand between files, and any
omission would silently produce wrong Fortran.

This is only ever an optimisation. Anything it skips, or fails to produce, is
left for the ordinary per-file make rules, which invoke the real ``psyclone``
command. A build can therefore never be broken by this driver: at worst it is
as slow as it was before.
"""

import argparse
import os
import sys
import tempfile
import time


# Files with a hand-written PSy layer in SOURCE_DIR/psy are left entirely to
# make, whose rule deletes the generated layer afterwards.
PSY_OVERRIDE_DIR = "psy"


def _parse_arguments(argv):
    """
    Interpret the command line.

    :param list argv: arguments, excluding the program name.
    :returns: the parsed arguments.
    :rtype: argparse.Namespace
    """
    parser = argparse.ArgumentParser(
        description="Transform many LFRic algorithm files in one process.")
    parser.add_argument("--source-dir", required=True,
                        help="directory holding the original source")
    parser.add_argument("--working-dir", required=True,
                        help="workspace holding preprocessed .x90 files")
    parser.add_argument("--optimisation-path", default="",
                        help="root of the transformation script tree")
    parser.add_argument("--dsl", default="psykal",
                        help="DSL subdirectory of the optimisation path")
    parser.add_argument("--config", default="",
                        help="PSyclone configuration file")
    parser.add_argument("--workers", type=int, default=0,
                        help="maximum concurrent transformations")
    parser.add_argument("--extra", default="",
                        help="additional PSyclone options, space separated")
    parser.add_argument("--verbose", action="store_true",
                        help="report each file as it is transformed")
    return parser.parse_args(argv)


class Job:
    """
    One algorithm file to transform.

    :param str algorithm: the preprocessed ``.x90`` file in the workspace.
    :param str stem: path of the file relative to the workspace, without its
        extension. Used to derive the output names.
    :param recipe: transformation script to apply, if any.
    :type recipe: str or None
    """

    def __init__(self, algorithm, stem, recipe):
        self.algorithm = algorithm
        self.stem = stem
        self.recipe = recipe

    def outputs(self, working_dir):
        """
        Return the files PSyclone will produce.

        :param str working_dir: the workspace.
        :returns: (algorithm layer, PSy layer).
        :rtype: tuple[str, str]
        """
        return (os.path.join(working_dir, self.stem + ".f90"),
                os.path.join(working_dir, self.stem + "_psy.f90"))

    def argv(self, arguments):
        """
        Build the PSyclone argument list for this job.

        Mirrors the recipes in psyclone_psykal.mk exactly, so that a file
        transformed here is indistinguishable from one transformed by make.

        :param arguments: the parsed command line.
        :type arguments: argparse.Namespace
        :rtype: list
        """
        working = arguments.working_dir
        algorithm_out, psy_out = self.outputs(working)
        argv = ["-api", "lfric", "-d", working]
        if arguments.config:
            argv += ["--config", arguments.config]
        if self.recipe:
            argv += ["-s", self.recipe]
        argv += ["-okern", os.path.join(working, "kernel"),
                 "-oalg", algorithm_out,
                 "-opsy", psy_out]
        argv += arguments.extra.split()
        argv.append(self.algorithm)
        return argv


def _recipe_for(stem, arguments):
    """
    Find the transformation script make would use for an algorithm.

    A script named after the file wins; otherwise a ``global.py`` alongside it;
    otherwise no script at all.

    :param str stem: workspace-relative path without extension.
    :param arguments: the parsed command line.
    :type arguments: argparse.Namespace
    :returns: path of the script, or None.
    :rtype: str or None
    """
    if not arguments.optimisation_path:
        return None
    root = os.path.join(arguments.optimisation_path, arguments.dsl)
    specific = os.path.join(root, stem + ".py")
    if os.path.exists(specific):
        return specific
    shared = os.path.join(root, "global.py")
    if os.path.exists(shared):
        return shared
    return None


def _newest(*paths):
    """
    Return the most recent modification time of the given paths.

    :param paths: files to examine; missing ones are ignored.
    :returns: the newest mtime, or 0.0 if none of them exist.
    :rtype: float
    """
    times = []
    for path in paths:
        if path:
            try:
                times.append(os.stat(path).st_mtime)
            except OSError:
                pass
    return max(times) if times else 0.0


def _is_stale(job, arguments):
    """
    Decide whether a job needs to run, applying make's rules.

    :param Job job: the candidate.
    :param arguments: the parsed command line.
    :type arguments: argparse.Namespace
    :rtype: bool
    """
    algorithm_out, psy_out = job.outputs(arguments.working_dir)
    for output in (algorithm_out, psy_out):
        if not os.path.exists(output):
            return True
    inputs = _newest(job.algorithm, job.recipe, arguments.config)
    return inputs > min(_newest(algorithm_out), _newest(psy_out))


def _has_psy_override(stem, arguments):
    """
    Return True if a hand-written PSy layer exists for this algorithm.

    Those files are left to make, which invokes PSyclone and then deletes the
    generated PSy layer. Reproducing that dance here would risk getting the
    ordering wrong for no useful saving.

    :param str stem: workspace-relative path without extension.
    :param arguments: the parsed command line.
    :type arguments: argparse.Namespace
    :rtype: bool
    """
    override = os.path.join(arguments.source_dir, PSY_OVERRIDE_DIR,
                            os.path.basename(stem) + "_psy.f90")
    return os.path.exists(override)


def discover(arguments):
    """
    Find the algorithm files which need transforming.

    :param arguments: the parsed command line.
    :type arguments: argparse.Namespace
    :returns: the jobs to run.
    :rtype: list[Job]
    """
    jobs = []
    working = arguments.working_dir
    for directory, _sub_dirs, names in os.walk(working):
        for name in names:
            if not name.endswith(".x90"):
                continue
            algorithm = os.path.join(directory, name)
            stem = os.path.relpath(algorithm, working)[:-len(".x90")]
            if _has_psy_override(stem, arguments):
                continue
            job = Job(algorithm, stem, _recipe_for(stem, arguments))
            if _is_stale(job, arguments):
                jobs.append(job)
    # Deterministic order keeps logs comparable between builds.
    jobs.sort(key=lambda item: item.stem)
    return jobs


def _run_in_child(job, arguments, log_path):
    """
    Body of a forked child: transform one file and exit.

    Both file descriptors are redirected, not just ``sys.stdout``, so that
    anything written by an extension module is captured too.

    :param Job job: the file to transform.
    :param arguments: the parsed command line.
    :type arguments: argparse.Namespace
    :param str log_path: file to capture output in.
    """
    code = 1
    try:
        log = os.open(log_path, os.O_WRONLY | os.O_CREAT | os.O_TRUNC, 0o600)
        os.dup2(log, 1)
        os.dup2(log, 2)
        os.close(log)

        argv = job.argv(arguments)
        sys.argv = ["psyclone"] + argv
        from psyclone.generator import main as psyclone_main
        code = 0
        try:
            psyclone_main(argv)
        except SystemExit as exit_error:
            value = exit_error.code
            code = 0 if value is None else (
                value if isinstance(value, int) else 1)
    except BaseException:  # pylint: disable=broad-except
        import traceback
        traceback.print_exc()
        code = 1
    finally:
        try:
            sys.stdout.flush()
            sys.stderr.flush()
        except Exception:  # pylint: disable=broad-except
            pass
        # _exit, not sys.exit: the child must not run the parent's atexit
        # handlers nor flush buffers it inherited.
        os._exit(0 if code == 0 else 1)  # pylint: disable=protected-access


def _report(job, log_path, failed):
    """
    Copy a child's captured output to the build log.

    :param Job job: the file that was transformed.
    :param str log_path: file holding the captured output.
    :param bool failed: True if the transformation failed.
    """
    try:
        with open(log_path, encoding="utf8", errors="replace") as handle:
            output = handle.read()
    except OSError:
        output = ""
    if failed:
        sys.stderr.write(
            f"PSyclone failed for {job.algorithm}\n{output}")
    elif output.strip():
        sys.stdout.write(output)


def run(jobs, arguments):
    """
    Transform every job, several at a time, in forked children.

    :param list jobs: the work to do.
    :param arguments: the parsed command line.
    :type arguments: argparse.Namespace
    :returns: number of files which failed.
    :rtype: int
    """
    workers = arguments.workers or (os.cpu_count() or 1)
    workers = max(1, min(workers, len(jobs)))

    queue = list(jobs)
    running = {}
    failures = 0
    log_dir = tempfile.mkdtemp(prefix="psyclone-batch-")
    try:
        while queue or running:
            while queue and len(running) < workers:
                job = queue.pop(0)
                log_path = os.path.join(
                    log_dir, job.stem.replace(os.sep, "_") + ".log")
                pid = os.fork()
                if pid == 0:
                    _run_in_child(job, arguments, log_path)
                running[pid] = (job, log_path)
                if arguments.verbose:
                    print(f"  PSyclone {job.stem}.x90", flush=True)

            pid, status = os.wait()
            entry = running.pop(pid, None)
            if entry is None:
                continue
            job, log_path = entry
            failed = not (os.WIFEXITED(status) and os.WEXITSTATUS(status) == 0)
            if failed:
                failures += 1
            _report(job, log_path, failed)
    finally:
        # Never leave orphans behind if we are interrupted.
        for pid in running:
            try:
                os.kill(pid, 9)
            except OSError:
                pass
        import shutil
        shutil.rmtree(log_dir, ignore_errors=True)
    return failures


def main(argv=None):
    """
    Entry point.

    :param list argv: arguments (defaults to ``sys.argv[1:]``).
    :returns: process exit code.
    :rtype: int
    """
    arguments = _parse_arguments(sys.argv[1:] if argv is None else argv)

    jobs = discover(arguments)
    if not jobs:
        return 0

    started = time.monotonic()
    # The whole point: pay this once for the entire component.
    import psyclone.generator  # noqa: F401  (imported for side effects)
    imported = time.monotonic()

    failures = run(jobs, arguments)
    finished = time.monotonic()

    print(f"PSyclone: {len(jobs)} algorithm file(s), "
          f"{imported - started:.1f}s to load PSyclone, "
          f"{finished - imported:.1f}s to transform", flush=True)

    if failures:
        # Say nothing more: make will retry each file individually through the
        # ordinary rules and report the failure itself.
        sys.stderr.write(
            f"PSyclone: {failures} file(s) could not be transformed in "
            f"batch; make will retry them individually\n")
    return 0


if __name__ == "__main__":
    sys.exit(main())

