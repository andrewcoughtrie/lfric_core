# -----------------------------------------------------------------------------
#  (C) Crown copyright 2026 Met Office. All rights reserved.
#  The file LICENCE, distributed with this code, contains details of the terms
#  under which the code may be used.
# -----------------------------------------------------------------------------
"""
Persistent PSyclone server for the LFRic build system.

The server imports PSyclone *once*, in the dispatcher process, and then forks a
fresh child per job. Each child inherits the fully-imported interpreter through
copy-on-write, so a job starts in a few milliseconds instead of paying the
several-second cost of loading Python, PSyclone, fparser and sympy from disk.

Why fork-per-job rather than a reusable worker pool
---------------------------------------------------
PSyclone keeps a good deal of process-global state: ``Config._instance``,
``LFRicConstants.HAS_BEEN_INITIALISED``, ``LFRicTypes._name_to_class``,
``SymbolicMaths._instance``, ``ModuleManager._instance``,
``LFRicBuiltinFunctorFactory._instance`` and fparser's ``SYMBOL_TABLES``, to
name only those that exist today. A long-lived worker would have to reset all
of it between jobs, and *any* omission silently produces wrong Fortran rather
than an error. That list would also have to be maintained against PSyclone's
internals in perpetuity.

Forking per job side-steps the problem completely: the child starts from a
pristine copy of the parent's address space, and the parent is careful never to
run PSyclone itself - it only imports it. Isolation is therefore exactly as
strong as running a separate ``psyclone`` process, while the fork costs a few
tens of milliseconds against roughly nine seconds for a cold interpreter, so
essentially all of the saving is retained.

Protocol (per job)
------------------
* client writes ``<job-dir>\\n`` to the shared request FIFO. The message is a
  single short line, comfortably inside ``PIPE_BUF``, so concurrent writes from
  many clients cannot interleave;
* the dispatcher reads the line and forks a child once a slot is free;
* the child reads ``<job-dir>/request.json``, runs PSyclone and writes the
  result to the ``<job-dir>/response`` FIFO;
* the client reads the response FIFO, replays the captured output and exits
  with the returned code.

Lifetime
--------
The server shuts itself down after ``PSYCLONE_SERVER_IDLE_TIMEOUT`` seconds
without work, so no explicit stop step is required from the build. More
importantly its lifetime is *pinned to the owning make process*
(``PSYCLONE_OWNER_PID``/``PSYCLONE_OWNER_START``, resolved by the client): the
dispatch loop polls that process once a second and exits as soon as it goes
away. A build that finishes normally therefore takes its server with it, and
one that is killed manually - with SIGINT, SIGTERM or SIGKILL, at any level of
recursive make - never leaves an orphaned server behind.
"""

import errno
import json
import os
import select
import shutil
import signal
import sys
import time
import traceback

try:
    import psyclone_procs
except ImportError:  # pragma: no cover - executed from an unusual sys.path
    sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
    import psyclone_procs


# Number of seconds of inactivity after which the server exits.
DEFAULT_IDLE_TIMEOUT = 300.0

# How long a child will wait for its client to start reading the response
# FIFO before abandoning the result. Opening a FIFO for writing blocks until a
# reader appears, so without this a client that was killed mid-job - which is
# what happens to every job in flight when a build is interrupted - would leave
# the child wedged for ever.
RESPONSE_TIMEOUT = float(os.environ.get("PSYCLONE_RESPONSE_TIMEOUT") or 60.0)

# Interval between attempts to open the response FIFO.
RESPONSE_POLL_INTERVAL = 0.05


def _open_response_fifo(path, timeout=RESPONSE_TIMEOUT):
    """
    Open a response FIFO for writing without blocking indefinitely.

    ``O_WRONLY`` on its own blocks until a reader appears and can therefore
    hang for ever if the client has gone. Adding ``O_NONBLOCK`` makes the open
    fail with ``ENXIO`` while there is no reader, which lets us poll until the
    deadline expires and then give up.

    :param str path: the response FIFO.
    :param float timeout: seconds to keep trying for.
    :returns: a writable file descriptor, or None if no reader ever appeared.
    :rtype: int or None
    """
    deadline = time.monotonic() + timeout
    while True:
        try:
            return os.open(path, os.O_WRONLY | os.O_NONBLOCK)
        except OSError as error:
            if error.errno != errno.ENXIO:
                # The FIFO has gone, or something else is wrong. Either way the
                # client is no longer interested.
                return None
            if time.monotonic() >= deadline:
                return None
            time.sleep(RESPONSE_POLL_INTERVAL)


def _write_response(job_dir, returncode, stdout, stderr):
    """
    Write a job result to the per-job response FIFO.

    :param str job_dir: the job directory.
    :param int returncode: PSyclone's exit code.
    :param str stdout: captured standard output.
    :param str stderr: captured standard error.
    """
    payload = json.dumps(
        {"returncode": returncode, "stdout": stdout, "stderr": stderr})
    handle = _open_response_fifo(os.path.join(job_dir, "response"))
    if handle is None:
        # The client gave up - it may have fallen back to a direct invocation,
        # or the whole build may have been interrupted. Nothing left to do.
        return
    try:
        data = payload.encode("utf8")
        # The reader drains in chunks, so keep writing until it has all gone.
        while data:
            try:
                written = os.write(handle, data)
            except BlockingIOError:
                select.select([], [handle], [], 1.0)
                continue
            except OSError:
                # Reader vanished part way through; the client will time out
                # and fall back to a direct invocation.
                return
            data = data[written:]
    finally:
        os.close(handle)


def run_job(job_dir):
    """
    Execute a single PSyclone job. Only ever called in a freshly forked child.

    Because the child exits as soon as the job is done there is no need to save
    and restore the working directory, ``sys.argv``, ``sys.path`` or any of
    PSyclone's global state - the next job gets a brand new copy of the parent.

    :param str job_dir: directory containing ``request.json`` and the
        ``response`` FIFO.
    :returns: the exit code PSyclone produced.
    :rtype: int
    """
    from io import StringIO

    out, err = StringIO(), StringIO()
    returncode = 0
    try:
        with open(os.path.join(job_dir, "request.json"),
                  encoding="utf8") as handle:
            request = json.load(handle)

        os.chdir(request["cwd"])
        # Make the recipe search locations importable - PYTHONPATH from the
        # client, e.g. LFRIC_BUILD/psyclone for psyclone_tools.
        for path in reversed(request.get("sys_path", [])):
            if path and path not in sys.path:
                sys.path.insert(0, path)

        argv = request["argv"]
        sys.argv = ["psyclone"] + argv
        sys.stdout, sys.stderr = out, err

        from psyclone.generator import main as psyclone_main
        try:
            psyclone_main(argv)
        except SystemExit as exit_error:
            code = exit_error.code
            returncode = 0 if code is None else (
                code if isinstance(code, int) else 1)
    except Exception:  # pylint: disable=broad-except
        traceback.print_exc(file=err)
        returncode = 1
    finally:
        sys.stdout, sys.stderr = sys.__stdout__, sys.__stderr__

    _write_response(job_dir, returncode, out.getvalue(), err.getvalue())
    return returncode


class PsycloneServer:
    """
    The dispatcher process: owns the request FIFO, forks a child per job and
    handles idle-timeout based shutdown.

    :param str server_dir: directory holding the request FIFO and job dirs.
    :param int workers: maximum number of jobs to run concurrently.
    :param float idle_timeout: seconds of inactivity before shutting down.
    :param owner_pid: pid of the owning make process. When it disappears the
        server shuts down promptly, so that a finished build - or one killed
        manually with SIGINT, SIGTERM or even SIGKILL - leaves no orphaned
        servers behind.
    :type owner_pid: int or None
    :param owner_start: the owner's process start time, recorded when it was
        resolved, used to detect pid reuse.
    :type owner_start: str or None
    :param bool remove_dir: True if ``server_dir`` was created for this build
        alone and so should be deleted on shutdown.
    """

    def __init__(self, server_dir, workers, idle_timeout, owner_pid=None,
                 owner_start=None, remove_dir=False):
        self._server_dir = server_dir
        self._workers = max(1, int(workers))
        self._idle_timeout = float(idle_timeout)
        self._owner_pid = int(owner_pid) if owner_pid else None
        self._owner_start = owner_start or None
        self._remove_dir = remove_dir
        self._request_fifo = os.path.join(server_dir, "request.fifo")
        self._pid_file = os.path.join(server_dir, "server.pid")
        self._ready_file = os.path.join(server_dir, "server.ready")
        self._pending = []
        self._active = set()
        self._stopping = False
        self._fifo_fds = ()

    # -- start up -----------------------------------------------------------
    @staticmethod
    def _preload():
        """
        Import PSyclone into the dispatcher so that every child inherits it.

        This is the whole point of the server: the import costs several seconds
        and many thousands of filesystem operations, and doing it here means it
        is paid exactly once per build rather than once per algorithm file.
        """
        import psyclone.generator  # noqa: F401  (imported for side effects)

    def _create_request_fifo(self):
        """Create the shared request FIFO if it does not already exist."""
        try:
            os.mkfifo(self._request_fifo)
        except FileExistsError:
            pass

    def _signal_ready(self):
        """
        Announce that the server is accepting work.

        Called only once PSyclone is resident and the FIFO is open, so a client
        which sees this file can genuinely be served straight away.
        """
        with open(self._ready_file, "w", encoding="utf8") as handle:
            handle.write(str(os.getpid()))

    def _install_signal_handlers(self):
        """Break the dispatch loop cleanly on termination signals."""
        def _handler(_signum, _frame):
            self._stopping = True

        for signal_name in ("SIGTERM", "SIGINT", "SIGHUP"):
            signal_number = getattr(signal, signal_name, None)
            if signal_number is not None:
                signal.signal(signal_number, _handler)

    def _owner_alive(self):
        """
        Return True if the owning make process is still running.

        When no owner was supplied the server relies solely on the idle
        timeout, so it is reported as alive. A process that has become a
        zombie (killed but not yet reaped by its parent) is treated as dead,
        so the server does not linger during that window, as is a pid that has
        since been reused by an unrelated process.

        :rtype: bool
        """
        return psyclone_procs.owner_alive(self._owner_pid, self._owner_start)

    # -- job handling -------------------------------------------------------
    def _spawn(self, job_dir):
        """
        Fork a child to run one job.

        :param str job_dir: the job directory to hand to the child.
        """
        pid = os.fork()
        if pid == 0:
            # -- child ------------------------------------------------------
            code = 1
            try:
                # Restore default signal handling; the parent's handlers only
                # make sense for the dispatch loop.
                for name in ("SIGTERM", "SIGINT", "SIGHUP"):
                    number = getattr(signal, name, None)
                    if number is not None:
                        signal.signal(number, signal.SIG_DFL)
                # Do not keep the shared request FIFO open in the child.
                for descriptor in self._fifo_fds:
                    try:
                        os.close(descriptor)
                    except OSError:
                        pass
                code = run_job(job_dir)
            except BaseException:  # pylint: disable=broad-except
                try:
                    traceback.print_exc(file=sys.__stderr__)
                except Exception:  # pylint: disable=broad-except
                    pass
            finally:
                # _exit, not sys.exit: the child must not run the parent's
                # atexit handlers nor flush its buffers.
                os._exit(0 if code == 0 else 1)  # pylint: disable=W0212
        self._active.add(pid)

    def _reap(self):
        """Collect any children which have finished."""
        for pid in list(self._active):
            try:
                done, _status = os.waitpid(pid, os.WNOHANG)
            except ChildProcessError:
                self._active.discard(pid)
                continue
            if done:
                self._active.discard(pid)

    def _start_pending(self):
        """Fork children for queued jobs while there is spare capacity."""
        while self._pending and len(self._active) < self._workers:
            self._spawn(self._pending.pop(0))

    # -- shutdown -----------------------------------------------------------
    def _shutdown(self):
        """Stop all children promptly and clean up the server files."""
        # Remove the coordination files first so that clients immediately see
        # the server as gone - and a new build can start a fresh one - even if
        # reaping the children takes a moment.
        for path in (self._ready_file, self._pid_file, self._request_fifo):
            try:
                os.remove(path)
            except OSError:
                pass

        # Terminate any running jobs directly so that shutdown is bounded and
        # cannot hang if a child is busy inside PSyclone.
        for pid in list(self._active):
            try:
                os.kill(pid, signal.SIGTERM)
            except OSError:
                self._active.discard(pid)

        deadline = time.monotonic() + 2.0
        while self._active and time.monotonic() < deadline:
            self._reap()
            if self._active:
                time.sleep(0.05)

        for pid in list(self._active):
            try:
                os.kill(pid, signal.SIGKILL)
                os.waitpid(pid, 0)
            except OSError:
                pass
            self._active.discard(pid)

        # A directory created for this build alone goes with it, leaving no
        # trace of the server once make has finished.
        if self._remove_dir:
            shutil.rmtree(self._server_dir, ignore_errors=True)

    # -- main loop ----------------------------------------------------------
    def serve(self):
        """Run the dispatch loop until the build ends or the server idles."""
        self._create_request_fifo()
        self._install_signal_handlers()

        # Pay the expensive import once, before anything can ask for work.
        self._preload()

        # Open the read end first (non-blocking) so that opening the write end
        # does not deadlock waiting for a reader. The write handle is then kept
        # open for the lifetime of the server so the read end never sees EOF
        # between clients; reads block (with a timeout) rather than spinning.
        reader_fd = os.open(self._request_fifo, os.O_RDONLY | os.O_NONBLOCK)
        writer_fd = os.open(self._request_fifo, os.O_WRONLY)
        self._fifo_fds = (reader_fd, writer_fd)

        # Only advertise readiness once we can actually accept work.
        self._signal_ready()
        try:
            self._dispatch_loop(reader_fd)
        finally:
            for descriptor in self._fifo_fds:
                try:
                    os.close(descriptor)
                except OSError:
                    pass
            self._fifo_fds = ()
            self._shutdown()

    def _dispatch_loop(self, reader_fd):
        """
        Read enqueue messages and fork a child for each job.

        :param int reader_fd: the read end of the shared request FIFO.
        """
        buffer = b""
        last_activity = time.monotonic()
        while not self._stopping:
            try:
                ready, _, _ = select.select([reader_fd], [], [], 1.0)
            except InterruptedError:
                # A signal interrupted the wait; re-check the stop flag.
                continue
            if ready:
                chunk = os.read(reader_fd, 65536)
                if chunk:
                    buffer += chunk
                    while b"\n" in buffer:
                        line, buffer = buffer.split(b"\n", 1)
                        job_dir = line.decode("utf8").strip()
                        if job_dir == "__STOP__":
                            return
                        if job_dir:
                            self._pending.append(job_dir)

            self._reap()
            self._start_pending()

            if self._pending or self._active:
                # Work in progress counts as activity, so a long build never
                # trips the idle timeout.
                last_activity = time.monotonic()

            # Shut down promptly if the owning build has gone away, otherwise
            # fall back to the inactivity timeout.
            if not self._owner_alive():
                return
            if time.monotonic() - last_activity > self._idle_timeout:
                return


def main():
    """Command-line entry point used by the auto-start logic in the client."""
    server_dir = os.environ["PSYCLONE_SERVER_DIR"]
    workers = os.environ.get("PSYCLONE_WORKERS") or os.cpu_count() or 1
    idle_timeout = os.environ.get(
        "PSYCLONE_SERVER_IDLE_TIMEOUT", DEFAULT_IDLE_TIMEOUT)
    owner_pid = os.environ.get("PSYCLONE_OWNER_PID")
    owner_start = os.environ.get("PSYCLONE_OWNER_START")
    remove_dir = os.environ.get("PSYCLONE_SERVER_DIR_TRANSIENT") == "1"
    server = PsycloneServer(server_dir, workers, idle_timeout, owner_pid,
                            owner_start, remove_dir)
    server.serve()


if __name__ == "__main__":
    main()


