# Multi Dataset Load Refresh Contention Fix 2026-02-20

## Summary
Addressed a multi-dataset loading hang by preventing dataset-list refresh queries from running concurrently with active dataset/transformation loading.

## Root Cause
- Dataset list refresh observers queried DB repeatedly while a multi-dataset load was in progress.
- `confirmPrior` set `currentDatasetName` early, which triggered another refresh observer before load completion.
- Concurrent DB reads on the same connection increased contention and could stall perceived progress.

## Fix
- Added `should_refresh_dataset_tables(dataset_loading, transformation_loading)` helper.
- Paused periodic and `currentDatasetName`-triggered dataset table refresh while loading flags are active.
- Added re-entry guard to block duplicate `confirmPrior` clicks during active load.
- Deferred `rvals$currentDatasetName <- selected_datasets` until load workflow completes.
- Hardened modal lifecycle helpers with idempotent guards.
- Replaced post-modal `req(length(selected_datasets) > 0)` with explicit validation + return path so the loading modal cannot be left open by a silent `req` abort.
- Added `normalize_selected_datasets()` to centralize blank/NA deduping.

## Validation
- `parse(file='R/datainputTab.R')` pass.
- `testthat::test_file('tests/testthat/test-dataset-load-timeout-helpers.R')` pass.
- `testthat::test_file('tests/testthat/test-visualize-layout-default.R')` pass.

## Related
- [[Multi_Dataset_Confirm_Hang_Fix_2026-02-19]]
- [[Workflows_MOC]]
- [[Interaction_Log_2026-02-20]]
