# PSyclone build acceleration

Processing an LFRic build's algorithm layer means running PSyclone once per
`.x90` file. A large build has more than 800 of them, and each invocation pays
the cost of starting a fresh Python interpreter and importing PSyclone:

| Cost per invocation | Measured |
| ------------------- | -------- |
| `import psyclone.generator` | 7-10 s |
| ...of which `sympy` | ~5 s |
| Filesystem metadata operations | ~16,500 |
| `sys.path` entries searched | 54, all on shared NFS |

For 800 files that is roughly two hours of wall time and thirteen million NFS
metadata operations - repeated by every build running concurrently on the same
site install. This directory contains the machinery that removes that cost.

## How it works

`psyclone_client.py` is a drop-in replacement for the `psyclone` command. It is
what `psyclone_psykal.mk` invokes via the `PSYCLONE` variable. Rather than
starting an interpreter it hands the arguments to a long-lived server:

```
make -j  ->  psyclone_client.py  --(request FIFO)-->  psyclone_server.py
                     ^                                       |
                     |                                    fork()
                     +-----(per-job response FIFO)----  child runs PSyclone
```

`psyclone_server.py` imports PSyclone **once** and then forks a fresh child for
each job. The child inherits the fully-imported interpreter through
copy-on-write, so it starts in milliseconds.

### Why fork per job rather than reuse workers

PSyclone holds a lot of process-global state - `Config._instance`,
`LFRicConstants.HAS_BEEN_INITIALISED`, `LFRicTypes._name_to_class`,
`SymbolicMaths._instance`, `ModuleManager._instance`,
`LFRicBuiltinFunctorFactory._instance`, fparser's `SYMBOL_TABLES`, and more.
A reusable worker would have to reset every one of them between jobs, and any
omission would silently emit **wrong Fortran** rather than raise an error. That
list would also have to track PSyclone's internals for ever.

Forking per job avoids the question entirely: each job begins from a pristine
copy of a parent that has only ever imported PSyclone, never run it. Isolation
is exactly as strong as a separate process, at a fraction of the cost.

### Lifetime

The server is deliberately detached from the make recipe that starts it, so it
needs another way to know when to stop. `lfric.mk` exports
`PSYCLONE_OWNER_PID` - the pid of the *top-level* make - and the server polls
it once a second, exiting as soon as it disappears. That covers a normal
finish and a build killed with SIGINT, SIGTERM or SIGKILL at any level of
recursive make. If the variable is unset, `psyclone_procs.py` finds the
outermost make in the client's own ancestry.

Watching make itself matters: under the non-interactive shells used by
rose-stem and cylc, make is *not* the process group leader, so a server
watching the process group would outlive a manually killed build.

An idle timeout provides a final backstop.

## Environment variables

| Variable | Default | Purpose |
| -------- | ------- | ------- |
| `PSYCLONE_SERVER_DISABLE` | unset | Set to any value to bypass the server entirely and call `psyclone` directly. First thing to try when debugging. |
| `PSYCLONE_WORKERS` | `MAKE_THREADS`, else `nproc`, capped at `PSYCLONE_MAX_WORKERS` | Maximum concurrent jobs. Each needs roughly 100MB. |
| `PSYCLONE_MAX_WORKERS` | 8 | Cap applied by `psyclone_psykal.mk`. |
| `PSYCLONE_SERVER_DIR` | `$TMPDIR/psyclone-server-<uid>-<make pid>` | Coordination directory. Set explicitly to pin it somewhere. |
| `PSYCLONE_SERVER_IDLE_TIMEOUT` | 300 | Seconds of inactivity before the server exits. |
| `PSYCLONE_JOB_TIMEOUT` | 900 | Seconds a client waits for one job. |
| `PSYCLONE_RESPONSE_TIMEOUT` | 60 | Seconds a child waits for its client to collect the result. |
| `PSYCLONE_SERVER_VERBOSE` | 1 | Set to `0` to silence fallback warnings. |
| `PSYCLONE_OWNER_PID` | set by `lfric.mk` | Process the server's lifetime is pinned to. |

To bypass the server for a single build:

```sh
make PSYCLONE=psyclone            # or
PSYCLONE_SERVER_DISABLE=1 make
```

## Failure behaviour

The client never breaks a build. If the server cannot be started, times out, or
violates the protocol, the client runs the real `psyclone` binary instead and
prints a one-line explanation to stderr. If builds are unexpectedly slow, look
for `psyclone_client:` messages in the log - they mean the fast path is not
being taken.

## Debugging

```sh
# Is a server running for this build?
ls $TMPDIR/psyclone-server-$(id -u)-*

# Watch what it is doing (children are transient, one per job).
pstree -p $(cat $TMPDIR/psyclone-server-$(id -u)-*/server.pid)

# Reproduce a failure without the server in the way.
PSYCLONE_SERVER_DISABLE=1 make ...
```

Stale directories from builds killed with SIGKILL are swept by the next client
that starts a server.

## Tests

```sh
cd tests && make server-smoke
```

`tests/server_smoke_test.py` checks that:

1. output is byte-identical to a direct `psyclone` invocation;
2. consecutive jobs are isolated, even when their optimisation recipes share
   the module basename `global`;
3. concurrent jobs are isolated and all are served correctly;
4. the server holds no idle workers, proving PSyclone was imported once;
5. an abandoned client does not cost a job slot permanently;
6. the server dies with its make process, even when the surrounding process
   group survives.

Note that `tests/Makefile` is not currently invoked by rose-stem or by any
GitHub workflow, so these tests must be run by hand.

