# Dataset Selector Hard Refresh Initialization and Preference Upsert Fix 2026-02-21

## Context
- On hard refresh, first-run dataset confirm could stall while a second refresh succeeded.
- Startup also depended on continuous table polling and whole-table preference rewrites.

## Root Cause
- Preference writes used `DBI::dbWriteTable(..., overwrite = TRUE)` for `lastOpenedDataset`, which can cause metadata-lock contention and is heavyweight.
- Dataset selector readiness relied on periodic polling instead of deterministic auth/session initialization, creating a first-load race.
- Session DB connections were not explicitly closed on session end.

## Changes
- Added shared preference helpers in `R/userPreferences.R`:
  - `read_user_preferences_safe()`
  - `get_user_preference_safe()`
  - `write_user_preference_safe()`
- Replaced overwrite-based preference writes with update/insert semantics in:
  - `R/datainputTab.R`
  - `R/DataLoader.R`
  - `inst/app/server.R` (theme preference path)
- Refactored dataset list refresh in `R/datainputTab.R`:
  - Removed periodic `invalidateLater` polling for dataset tables.
  - Added auth-driven and event-driven refresh (`refresh_dataset_tables()`).
  - Added selector readiness/refreshing gates before confirm can run.
  - Added stale-selection detection with forced refresh and user warning.
  - Added lightweight retry for `dbListTables()` failures.
- Added explicit session-end DB disconnect in `inst/app/server.R`.

## Validation
- `Rscript -e "devtools::test(filter='datainput-prior-datasets|user-preferences')"` -> `FAIL 0 | WARN 0 | SKIP 0`.
- `Rscript -e "devtools::test()"` -> `FAIL 0 | WARN 0 | SKIP 1 | PASS 283`.
- Added regression tests:
  - Deferred username initialization still permits confirm load (`tests/testthat/test-datainput-prior-datasets.R`).
  - Preference update/create behavior (`tests/testthat/test-user-preferences.R`).

## Files
- `R/userPreferences.R`
- `R/datainputTab.R`
- `R/DataLoader.R`
- `inst/app/server.R`
- `tests/testthat/test-datainput-prior-datasets.R`
- `tests/testthat/test-user-preferences.R`
