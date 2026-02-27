# Multi Dataset Load UI Performance Optimization 2026-02-19

## Summary
After fixing persisted-transformation loading for multi-dataset confirm, the app could still appear hung due to expensive full-data numeric inference performed repeatedly in Data Input UI render paths.

## Root Cause
- `R/datainputTab.R` used `mutate_all(as.numeric)` over the full `importedData` frame in multiple renderers (`attr` and `chem` UIs).
- On combined datasets, this repeated coercion can become a major UI-thread bottleneck.

## Fix
- Added `guess_numeric_columns_fast()` in `R/columnTypeHints.R`:
  - sample-based numeric-like column detection with configurable parse threshold.
- Added cached numeric-column hints inside `dataInputServer()` and reused them in:
  - `output$attr`
  - `output$chemUI`
  - ratio-source numeric helper logic

## Tests
- Added `tests/testthat/test-column-type-hints.R` for:
  - numeric-like detection behavior
  - safe handling of empty/invalid inputs

## Related
- [[Multi_Dataset_Confirm_Hang_Fix_2026-02-19]]
- [[Data_Ingestion_and_Preparation]]
- [[Interaction_Log_2026-02-19]]
