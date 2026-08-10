# Dataset Table Name Length Hardening 2026-02-20

## Summary
Prevented dataset save failures caused by overlong table names by introducing deterministic name shortening for persisted dataset tables.

## Changes
- Added `R/datasetNameHelpers.R`:
  - `dataset_table_name_hash()`
  - `build_dataset_table_name(username, dataset_label, max_len = 54)`
- Updated upload persistence path in `R/DataLoader.R` to use `build_dataset_table_name()`.
- Updated merge persistence path in `R/datainputTab.R` to use `build_dataset_table_name()`.
- Added user notification when a submitted name is shortened for storage.

## Why 54
- Metadata tables append `_metadata` (9 chars).
- Keeping base dataset table names at `<=54` keeps corresponding metadata table names at `<=63`, matching common DB identifier limits.

## Validation
- Parsed modified files (`R/datasetNameHelpers.R`, `R/DataLoader.R`, `R/datainputTab.R`).
- Added and ran `tests/testthat/test-dataset-name-helpers.R`.

## Related
- [[Persistence_MOC]]
- [[Interaction_Log_2026-02-20]]
