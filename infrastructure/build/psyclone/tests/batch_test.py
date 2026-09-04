# -----------------------------------------------------------------------------
#  (C) Crown copyright 2026 Met Office. All rights reserved.
#  The file LICENCE, distributed with this code, contains details of the terms
#  under which the code may be used.
# -----------------------------------------------------------------------------
"""
Tests for the batch PSyclone driver (psyclone_batch.py).

The driver is an optimisation layered underneath make's ordinary per-file
rules, so the properties that matter are:

1. *Fidelity* - a file transformed in batch is byte-for-byte identical to one
   transformed by invoking the ``psyclone`` command directly.
2. *Isolation* - files sharing a transformation script basename, or using
   differently-behaving scripts, do not contaminate one another. Forking per
   file is what guarantees this, since PSyclone keeps a lot of global state.
3. *Staleness* - the driver rebuilds exactly what make would rebuild, and
   nothing else, so that incremental builds stay incremental.
4. *Containment* - a file that cannot be transformed does not fail the build;
   it is simply left for make to retry individually.

The test can be run directly (``python batch_test.py``) or under pytest.
"""

import os
import shutil
import subprocess
import sys
import tempfile
import time


HERE = os.path.dirname(os.path.abspath(__file__))
# infrastructure/build/psyclone/tests -> repository root (develop).
REPO_ROOT = os.path.realpath(os.path.join(HERE, "..", "..", "..", ".."))
PSYCLONE_DIR = os.path.realpath(os.path.join(HERE, ".."))
BATCH = os.path.join(PSYCLONE_DIR, "psyclone_batch.py")
CONFIG = os.path.join(REPO_ROOT, "etc", "psyclone.cfg")

sys.path.insert(0, PSYCLONE_DIR)
import psyclone_batch  # noqa: E402  (needs PSYCLONE_DIR on the path)


ALGORITHM = """\
module {name}_mod
  use constants_mod, only: r_def
  use field_mod,     only: field_type

  implicit none

contains

  subroutine {name}( one )
    implicit none
    type(field_type), intent(inout) :: one
    call invoke(setval_c(one, 1.0_r_def))
  end subroutine {name}

end module {name}_mod
"""

# A recipe that duplicates the loop body once (two copies).
RECIPE_DUPLICATE_ONCE = '''\
"""Duplicate the first loop body once."""
from psyclone.psyGen import InvokeSchedule


def trans(psyir):
    for subroutine in psyir.walk(InvokeSchedule):
        loop = subroutine.loops()[0]
        loop.loop_body.addchild(loop.loop_body[0].copy())
    return psyir
'''

# Same module basename, different behaviour: duplicates the body twice.
RECIPE_DUPLICATE_TWICE = '''\
"""Duplicate the first loop body twice."""
from psyclone.psyGen import InvokeSchedule


def trans(psyir):
    for subroutine in psyir.walk(InvokeSchedule):
        loop = subroutine.loops()[0]
        loop.loop_body.addchild(loop.loop_body[0].copy())
        loop.loop_body.addchild(loop.loop_body[0].copy())
    return psyir
'''

# A recipe that always fails, used to prove failures are contained.
RECIPE_BROKEN = '''\
"""Deliberately unusable recipe."""


def trans(psyir):
    raise RuntimeError("this recipe is broken on purpose")
'''


def _workspace(algorithms, recipes=None):
    """
    Build a source/workspace pair for the driver to operate on.

    :param dict algorithms: {name: recipe subdirectory or None}.
    :param dict recipes: {subdirectory: recipe text}.
    :returns: (root, source_dir, working_dir, optimisation_path).
    """
    root = tempfile.mkdtemp(prefix="psyclone-batch-test-")
    source = os.path.join(root, "source")
    working = os.path.join(root, "working")
    optimisation = os.path.join(root, "optimisation")
    for path in (source, working, os.path.join(working, "kernel")):
        os.makedirs(path, exist_ok=True)

    for subdirectory, text in (recipes or {}).items():
        directory = os.path.join(optimisation, "psykal", subdirectory)
        os.makedirs(directory, exist_ok=True)
        with open(os.path.join(directory, "global.py"), "w",
                  encoding="utf8") as handle:
            handle.write(text)

    for name in algorithms:
        # The driver consumes the preprocessed copy in the workspace.
        with open(os.path.join(working, f"{name}.x90"), "w",
                  encoding="utf8") as handle:
            handle.write(ALGORITHM.format(name=name))
    return root, source, working, optimisation


def _run_batch(source, working, optimisation, extra=()):
    """
    Run the batch driver over a workspace.

    :returns: CompletedProcess.
    """
    env = dict(os.environ)
    env["PYTHONPATH"] = os.pathsep.join(
        [PSYCLONE_DIR, env.get("PYTHONPATH", "")])
    command = [sys.executable, BATCH,
               "--source-dir", source,
               "--working-dir", working,
               "--optimisation-path", optimisation,
               "--config", CONFIG,
               "--workers", "4",
               "--extra", "-l all", *extra]
    return subprocess.run(command, capture_output=True, text=True, check=False,
                          env=env)


def _run_direct(working, name, recipe, optimisation):
    """Transform one file with the real psyclone command, as make would."""
    command = ["psyclone", "-api", "lfric", "-d", working,
               "--config", CONFIG]
    if recipe:
        command += ["-s", recipe]
    command += ["-okern", os.path.join(working, "kernel"),
                "-oalg", os.path.join(working, f"{name}_direct.f90"),
                "-opsy", os.path.join(working, f"{name}_direct_psy.f90"),
                "-l", "all",
                os.path.join(working, f"{name}.x90")]
    env = dict(os.environ)
    env["PYTHONPATH"] = os.pathsep.join(
        [PSYCLONE_DIR, env.get("PYTHONPATH", "")])
    result = subprocess.run(command, capture_output=True, text=True,
                            check=False, env=env, cwd=working)
    assert result.returncode == 0, (
        f"direct psyclone failed:\n{result.stdout}\n{result.stderr}")


def _read(path):
    """Return the contents of a file."""
    with open(path, encoding="utf8") as handle:
        return handle.read()


def test_batch_output_matches_direct_invocation():
    """A file transformed in batch is identical to one transformed by the
    psyclone command, which is what make would otherwise have run."""
    root, source, working, optimisation = _workspace(
        {"alpha": None}, {"": RECIPE_DUPLICATE_ONCE})
    try:
        result = _run_batch(source, working, optimisation)
        assert result.returncode == 0, result.stderr
        recipe = os.path.join(optimisation, "psykal", "global.py")
        _run_direct(working, "alpha", recipe, optimisation)

        assert _read(os.path.join(working, "alpha_psy.f90")) == \
            _read(os.path.join(working, "alpha_direct_psy.f90")), \
            "batch PSy layer differs from a direct invocation"
        assert _read(os.path.join(working, "alpha.f90")) == \
            _read(os.path.join(working, "alpha_direct.f90")), \
            "batch algorithm layer differs from a direct invocation"
    finally:
        shutil.rmtree(root, ignore_errors=True)


def test_files_in_one_batch_are_isolated():
    """Forking per file must keep PSyclone's global state from leaking between
    the files of a single batch."""
    root, source, working, optimisation = _workspace(
        {"one": None, "two": None})
    try:
        # Two subdirectories, each with its own "global.py", so the two files
        # are transformed by differently-behaving scripts of the same name.
        for name, text in (("one", RECIPE_DUPLICATE_ONCE),
                           ("two", RECIPE_DUPLICATE_TWICE)):
            directory = os.path.join(optimisation, "psykal")
            os.makedirs(directory, exist_ok=True)
            with open(os.path.join(directory, f"{name}.py"), "w",
                      encoding="utf8") as handle:
                handle.write(text)

        result = _run_batch(source, working, optimisation)
        assert result.returncode == 0, result.stderr

        one = _read(os.path.join(working, "one_psy.f90"))
        two = _read(os.path.join(working, "two_psy.f90"))
        assert one != two, "recipes with different behaviour agreed"

        # Compare each against a direct invocation to be sure neither was
        # contaminated by the other.
        for name, script in (("one", "one.py"), ("two", "two.py")):
            _run_direct(working, name,
                        os.path.join(optimisation, "psykal", script),
                        optimisation)
            assert _read(os.path.join(working, f"{name}_psy.f90")) == \
                _read(os.path.join(working, f"{name}_direct_psy.f90")), \
                f"{name} was contaminated by the other file in the batch"
    finally:
        shutil.rmtree(root, ignore_errors=True)


def test_only_stale_files_are_rebuilt():
    """Incremental builds must stay incremental: a second run with nothing
    changed does no work, and touching one input rebuilds only that file."""
    root, source, working, optimisation = _workspace(
        {"alpha": None, "beta": None}, {"": RECIPE_DUPLICATE_ONCE})
    try:
        assert _run_batch(source, working, optimisation).returncode == 0
        first = {name: os.stat(
            os.path.join(working, f"{name}_psy.f90")).st_mtime_ns
            for name in ("alpha", "beta")}

        # Nothing has changed, so nothing should be regenerated.
        time.sleep(0.01)
        result = _run_batch(source, working, optimisation)
        assert result.returncode == 0
        assert "algorithm file" not in result.stdout, (
            "batch did work when everything was up to date")
        for name, when in first.items():
            assert os.stat(os.path.join(
                working, f"{name}_psy.f90")).st_mtime_ns == when, \
                f"{name} was needlessly regenerated"

        # Touch one algorithm; only it should be rebuilt.
        time.sleep(0.01)
        os.utime(os.path.join(working, "alpha.x90"))
        assert _run_batch(source, working, optimisation).returncode == 0
        assert os.stat(os.path.join(
            working, "alpha_psy.f90")).st_mtime_ns != first["alpha"], \
            "a stale file was not rebuilt"
        assert os.stat(os.path.join(
            working, "beta_psy.f90")).st_mtime_ns == first["beta"], \
            "an up-to-date file was rebuilt"
    finally:
        shutil.rmtree(root, ignore_errors=True)


def test_a_failing_file_does_not_fail_the_batch():
    """The driver is an optimisation, never a gate. A file it cannot transform
    is left for make to retry and report, and its neighbours still get done."""
    root, source, working, optimisation = _workspace(
        {"good": None, "bad": None})
    try:
        directory = os.path.join(optimisation, "psykal")
        os.makedirs(directory, exist_ok=True)
        for name, text in (("good", RECIPE_DUPLICATE_ONCE),
                           ("bad", RECIPE_BROKEN)):
            with open(os.path.join(directory, f"{name}.py"), "w",
                      encoding="utf8") as handle:
                handle.write(text)

        result = _run_batch(source, working, optimisation)
        assert result.returncode == 0, (
            "a broken file must not fail the batch, so that make can retry "
            "it and produce the real error message")
        assert os.path.exists(os.path.join(working, "good_psy.f90")), \
            "a healthy file was not transformed alongside a broken one"
        assert not os.path.exists(os.path.join(working, "bad_psy.f90")), \
            "the broken file should not have produced output"
        assert "retry" in result.stderr, "the failure was not reported"
    finally:
        shutil.rmtree(root, ignore_errors=True)


def test_hand_written_psy_layers_are_left_to_make():
    """Algorithms with an override in SOURCE_DIR/psy are skipped: make invokes
    PSyclone and then deletes the generated layer, and reproducing that
    ordering here would gain nothing."""
    root, source, working, optimisation = _workspace(
        {"override_alg": None}, {"": RECIPE_DUPLICATE_ONCE})
    try:
        psy_dir = os.path.join(source, "psy")
        os.makedirs(psy_dir, exist_ok=True)
        with open(os.path.join(psy_dir, "override_alg_psy.f90"), "w",
                  encoding="utf8") as handle:
            handle.write("! hand written\n")

        class _Arguments:
            """Stand-in for the parsed command line."""
            source_dir = source
            working_dir = working
            optimisation_path = optimisation
            dsl = "psykal"
            config = CONFIG

        assert psyclone_batch.discover(_Arguments()) == [], \
            "an algorithm with a hand-written PSy layer was not skipped"
    finally:
        shutil.rmtree(root, ignore_errors=True)


if __name__ == "__main__":
    test_batch_output_matches_direct_invocation()
    test_files_in_one_batch_are_isolated()
    test_only_stale_files_are_rebuilt()
    test_a_failing_file_does_not_fail_the_batch()
    test_hand_written_psy_layers_are_left_to_make()
    print("PSyclone batch driver test passed")

