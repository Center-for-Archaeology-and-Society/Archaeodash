# Multi Dataset Load Collect Coercion Performance Fix 2026-02-20

## Summary
Reduced multi-dataset load stall risk by removing eager whole-table character coercion during database collection.

## Root Cause
- `load_selected_datasets_workspace()` collected each table and immediately ran `mutate_all(as.character)`.
- On large multi-dataset loads this forced expensive full-table type conversion and high memory churn before merge.

## Fix
- In `R/datasetWorkspaceLoader.R`, removed immediate `mutate_all(as.character)` after `collect()`.
- Kept rowid normalization and downstream merged row map behavior unchanged.
- Added stage-level app logs:
  - per-dataset collect start/end with row counts
  - combined row count after bind.

## Validation
- Parsed `R/datasetWorkspaceLoader.R` and `R/datainputTab.R`.
- Re-ran `tests/testthat/test-dataset-load-timeout-helpers.R`.

## Related
- [[Multi_Dataset_Load_Refresh_Contention_Fix_2026-02-20]]
- [[Workflows_MOC]]
- [[Interaction_Log_2026-02-20]]
