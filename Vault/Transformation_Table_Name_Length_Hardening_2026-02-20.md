# Transformation Table Name Length Hardening 2026-02-20

## Problem
- Live app still produced table-name length failures after dataset-name shortening fixes.
- Root cause was in transformation persistence table naming, not dataset upload/merge naming.

## Root Cause
- `transform_prefix()` was capped at 55 characters.
- Longest transformation table suffix is `_selected_all` (13 chars).
- Resulting table names could reach 68 chars (`55 + 13`), exceeding MySQL's 64-char identifier limit.

## Fix
- In `R/transformationPersistence.R`:
  - Added explicit limits/constants:
    - `transform_table_name_max_len <- 64L`
    - `transform_table_suffixes <- c(\"_selected\", \"_selected_all\", \"_pca\", \"_umap\", \"_lda\", \"_meta\")`
  - Added `transform_prefix_max_len()` to compute a safe prefix limit from suffix lengths.
  - Updated `transform_prefix()` to cap by `transform_prefix_max_len()` (51 with current suffix set).

## Validation
- Added/updated tests in `tests/testthat/test-transformation-persistence-helpers.R`:
  - Prefix length bounded by `transform_prefix_max_len()`.
  - All generated transformation table names (`prefix + suffix`) are `<= 64`.
- Ran:
  - `devtools::test(filter = 'transformation-persistence-helpers|datainput-prior-datasets')`
  - Pass: 32 tests, 0 failures.
- Runtime sanity check:
  - Prefix length now 51; max suffixed table length now 64.

## Live Deployment
- Copied fix into `../Archaeodash/R/transformationPersistence.R`.
- Installed in live container and restarted `archaeodash`.

## Related
- [[Persistence_MOC]]
- [[Interaction_Log_2026-02-20]]
