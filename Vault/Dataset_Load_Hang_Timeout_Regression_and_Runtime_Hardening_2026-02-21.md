# Dataset Load Hang Timeout Regression and Runtime Hardening 2026-02-21

## Summary
- Added a regression test that simulates a stalled dataset workspace load and verifies `confirmPrior` exits via timeout instead of hanging.
- Hardened beta app preference-table lookup in `inst/app/server.R` with a fallback naming path to avoid runtime failure if internal helper resolution fails.
- Re-applied writable sass cache directory permissions in beta container to prevent cache permission-related startup/runtime warnings.

## Changes
- `tests/testthat/test-datainput-prior-datasets.R`
  - Added `confirm prior load times out instead of hanging on stalled workspace load` test.
  - Uses namespace patching to force a 1-second timeout and a non-returning workspace loader, then asserts no dataset state is committed.
- `inst/app/server.R`
  - Wrapped preference table-name builder calls in `tryCatch` fallback (`<clean_username>_preferences`) in both read/write preference helpers.
- Runtime operations (`archaeodashbeta` container)
  - Created `/srv/shiny-server/inst/app/app_cache/sass` and set writable permissions.
  - Reinstalled local package and restarted `archaeodashbeta`.

## Validation
- Ran: `devtools::test(filter = 'datainput-prior-datasets|dataset-load-timeout-helpers')`
- Result: pass (`36` tests, `0` failures).
- Post-restart recent logs show no new `read_user_preferences`/`build_user_preferences_table_name` or `app_cache` permission errors.

## Related
- [[Workflows_MOC]]
- [[Interaction_Log_2026-02-21]]
