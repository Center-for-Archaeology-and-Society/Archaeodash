# Table Name Policy 32 and DB Migration 2026-02-20

## Request
- User requested reducing max table-name length to 32 chars in live `Archaeodash` and ensuring existing tables are updated.

## Code Changes
- `R/datasetNameHelpers.R`
  - Added `app_table_name_max_len <- 32L`.
  - Updated `build_dataset_table_name(..., max_len = app_table_name_max_len)`.
  - Added `build_user_preferences_table_name()` with deterministic hash compaction.
- `R/DataLoader.R`
  - Upload save path now uses `app_table_name_max_len`.
  - Preferences table uses `build_user_preferences_table_name()`.
- `R/datainputTab.R`
  - Merge save path now uses `app_table_name_max_len`.
  - Preference read/write paths use `build_user_preferences_table_name()`.
- `R/transformationPersistence.R`
  - Set `transform_table_name_max_len <- 32L`.
  - Kept `_transformations` suffix in `transform_index_table()` while shortening username portion with hash when needed.
  - Updated `transform_prefix()` to hash-compact long prefixes while respecting 32-char full table limits.
- `R/tableNameMigration.R` (new)
  - Added migration helpers to rename existing preference/index/transformation tables and overlong dataset base tables, and to update transformation index prefixes.
- `inst/app/server.R`
  - Runs one-time `migrate_table_names_to_32(con)` per worker startup.
  - Preference read/write switched to `build_user_preferences_table_name()`.

## Validation
- Targeted tests passed:
  - `dataset-name-helpers`
  - `transformation-persistence-helpers`
  - `datainput-prior-datasets`
  - `dataLoader`

## Live Deployment and Migration
- Installed updated package in `archaeodash` and restarted container.
- Ran live DB migration explicitly:
  - Renamed entries: `151`
  - Skipped: `0`
- Follow-up cleanup for orphan overlong transformation tables: renamed `5`.

## Post-Migration Audit
- `OVER64 = 0`
- `DATASET_BASE_OVER32 = 0`
- `TX_OVER32 = 0`
- Remaining `OVER32` tables are expected metadata companions (`<dataset>_metadata`) under current naming model.

## Related
- [[Persistence_MOC]]
- [[Interaction_Log_2026-02-20]]
