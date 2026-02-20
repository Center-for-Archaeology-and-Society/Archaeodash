# Dataset Selector Refresh and Multiselect 2026-02-19

## Summary
`Choose dataset` did not reliably refresh after import because table discovery was not being refreshed on a reactive cadence. The selector now supports multiple selections for dataset management workflows.

## Changes
- Added `get_user_dataset_tables()` in `R/datainputTab.R` to centralize dataset-table filtering.
- Updated table refresh to poll with `invalidateLater(1500)` and refresh on `rvals$currentDatasetName` changes.
- Changed `selectedDatasets` input from single-select to `multiple = TRUE`.
- Kept `Confirm dataset selection` behavior single-load by loading only the first selected dataset and showing a warning when multiple are selected.

## Current Merge/Rename Behavior
- Triggered by `input$mergeDatasetsconfirm` in `R/datainputTab.R`.
- Reads all selected dataset tables, row-binds them, removes duplicate rows, rebuilds `rowid`, and saves to `username_<mergeName>`.
- If one dataset is selected, behavior is effectively a copy/rename into the new name.
- Source datasets are not deleted.
- If destination exists, prompts for overwrite.

## Related
- [[Data_Ingestion_and_Preparation]]
- [[Workflows_MOC]]
- [[Interaction_Log_2026-02-19]]
