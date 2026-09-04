# PSyclone batch pre-pass

`psyclone_psykal.mk` normally invokes `psyclone` once per `.x90` algorithm
file. On this platform a single `import psyclone.generator` takes roughly
7-10s and performs many thousands of metadata lookups on shared NFS. For large
builds this dominates wall time.

This branch replaces the daemon/client approach with a simpler batch pre-pass:

- `psyclone_batch.py` scans `WORKING_DIR` for stale algorithm files;
- it imports PSyclone once in the parent process;
- it forks children to transform files in parallel (copy-on-write reuse of the
  imported interpreter);
- the normal per-file make rules still exist as fallback and correctness guard.

## Makefile flow

`psyclone` target now runs three phases in order:

1. `psyclone-preprocess` - create all `*.x90` files in `WORKING_DIR`;
2. `psyclone-batch` - optional pre-pass (`PSYCLONE_NO_BATCH=1` disables it);
3. `psyclone-generate` - standard per-file rules, which are mostly no-ops
   because outputs are already up to date.

This preserves existing behaviour while reducing repeated Python start-up cost.

## Key variables

- `PSYCLONE_BATCH` path to batch driver script.
- `PSYCLONE_NO_BATCH=1` disable pre-pass for debugging.
- `PSYCLONE_WORKERS` parallel children for the pre-pass.
- `PSYCLONE_MAX_WORKERS` cap on worker count (default `8`).
- `PSYCLONE` command used by fallback per-file rules (default `psyclone`).

## Safety model

The batch pre-pass is an optimisation only:

- if a file fails in batch, the driver logs it and exits `0`;
- make then retries that file through the standard rule;
- therefore a batch failure cannot fail the build on its own.

## Tests

`tests/batch_test.py` checks:

1. output fidelity against direct `psyclone`;
2. isolation between files in one batch;
3. stale-file detection for incremental rebuilds;
4. failure containment;
5. skipping files with hand-written `SOURCE_DIR/psy/*_psy.f90` overrides.

Run:

```bash
cd infrastructure/build/psyclone/tests
python3 batch_test.py
```

And an end-to-end invoke check:

```bash
cd infrastructure/build/psyclone/tests
REPO=/var/tmp/scratch/persistent/andrew.coughtrie/development/lfric/lfric_core/develop
make no-optimisation/invoke \
  LFRIC_BUILD=$REPO/infrastructure/build \
  CORE_ROOT_DIR=$REPO
```

