# Dataset Discovery Username Sanitization and Truncation Fix (2026-03-04)

## Context
- User reported that, after restart/cache clear, login worked but no prior stored datasets appeared.

## Root Cause
- Dataset discovery in `get_user_dataset_tables()` only matched table names with `^<raw_username>_`.
- Saved dataset table names are generated from sanitized usernames (`janitor::make_clean_names`), so usernames with special characters (for example `nick-digs`) do not match.
- Under 32-char table-name policy, long usernames can be truncated in stored table names, which also breaks strict raw-prefix matching.

## Implementation
- Added `dataset_username_table_prefixes()` helper in `R/datasetNameHelpers.R`.
- Updated `R/datainputTab.R` dataset listing to match tables against all valid username-derived prefixes:
  - raw lowercase username
  - sanitized username
  - truncation-safe sanitized username prefix for 32-char hashed names

## Validation
- Added/ran helper tests in `tests/testthat/test-transformation-persistence-helpers.R`.
- Direct DB validation confirmed restored visibility for:
  - special-character username prefix (`nick-digs` -> `nick_digs_*`)
  - truncated long-username prefix (`zz_final_17717023151327` -> `zz_final_1771702315132_*`)
- Deployed to beta container via `devtools::install_local()` and restarted `archaeodashbeta`.
