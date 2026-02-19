# Multi Dataset Workspace Writeback and Browse Load Mode 2026-02-19

## Summary
Selecting multiple datasets now builds a combined workspace while tracking each row's source dataset and original rowid. Autosave writes updates back to each corresponding source table instead of writing only to a single selected dataset.

## Key Changes
- `R/datainputTab.R`:
  - `Confirm dataset selection` now loads all selected datasets together.
  - Stores per-row source mapping in `rvals$currentDatasetRowMap` with `rowid`, `dataset_name`, `source_rowid`.
  - Uses vector `rvals$currentDatasetName` and dataset-key build from all selected datasets.
- `R/updateCurrent.R`:
  - Uses workspace `importedData` as save source.
  - If `currentDatasetRowMap` exists, splits rows by `dataset_name`, restores original `source_rowid` into `rowid`, and overwrites each original dataset table + metadata.
  - Falls back to single-dataset autosave when no source map exists.
- `R/DataLoader.R`:
  - Added `merge_loaded_data(existing_data, incoming_data, mode)`.
  - Added UI control `loadMode` with:
    - replace existing workspace data
    - add rows to existing workspace data
  - `loadData` now honors replace/add behavior and clears source-map binding for file-based workspace loads.

## Tests Added
- `tests/testthat/test-datainput-prior-datasets.R`:
  - Multi-select confirm combines rows and stores source map.
- `tests/testthat/test-updateCurrent-multi-dataset.R`:
  - Autosave split writes changes back to each source dataset table.
- `tests/testthat/test-dataLoader.R`:
  - `merge_loaded_data()` replace vs add behavior.

## Related
- [[Data_Ingestion_and_Preparation]]
- [[Workflows_MOC]]
- [[Interaction_Log_2026-02-19]]
