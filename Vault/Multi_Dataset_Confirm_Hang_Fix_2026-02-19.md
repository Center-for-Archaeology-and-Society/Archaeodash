# Multi Dataset Confirm Hang Fix 2026-02-19

## Summary
Confirming multiple selected datasets could appear to hang because the load path attempted to load persisted transformation snapshots for a composite dataset key, which can trigger expensive/colliding snapshot retrieval for combined workspaces.

## Fix
- Updated `R/datainputTab.R` in the `confirmPrior` observer:
  - keep persisted transformation loading only for single-dataset loads.
  - explicitly reset `rvals$transformations` and `rvals$activeTransformation` for multi-dataset combined workspace loads.

## Why
- Persisted transformations are keyed to a base dataset snapshot and are not a reliable fit for on-the-fly combined datasets.
- Skipping this step removes a heavy operation from the multi-dataset load path and avoids key-collision side effects.

## Tests
- Added regression test in `tests/testthat/test-datainput-prior-datasets.R`:
  - `confirm prior with multiple datasets does not load persisted transformations`

## Related
- [[Multi_Dataset_Workspace_Writeback_and_Browse_Load_Mode_2026-02-19]]
- [[Data_Ingestion_and_Preparation]]
- [[Interaction_Log_2026-02-19]]
