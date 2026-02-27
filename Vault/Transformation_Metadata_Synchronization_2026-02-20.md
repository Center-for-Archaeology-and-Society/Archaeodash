# Transformation Metadata Synchronization 2026-02-20

## Summary
Transformation snapshots now synchronize metadata from canonical dataset metadata (`importedData`) so row-level group edits propagate across all saved transformations for the same dataset workspace.

## What Changed
- Added `selectedDataAll` snapshot state as the unfiltered transformation base.
- Added `applyTransformationGroupFilter()` to derive `selectedData` from `selectedDataAll` using each snapshot's `attrGroups` and `attrGroupsSub`.
- Updated `refreshTransformationMetadata()` to refresh all snapshot metadata (including group columns) from `importedData`, then re-apply each snapshot's group filter.
- Updated transformation load flow to sync snapshot metadata before apply and recompute ordinations from the synced filtered data.
- Persisted `selectedDataAll` in a new optional DB table suffix `_selected_all`, with backward-compatible loading for older snapshots.

## Resulting Behavior
- Editing a grouping value for a row updates canonical metadata and propagates into every saved transformation snapshot for that dataset.
- Saved transformations retain independent group-column choice and group filtering while sharing synchronized row metadata.

## Related
- [[Persistence_Transformation_Storage_Model]]
- [[Edit_Assignment_and_RowID_Integrity]]
- [[Interaction_Log_2026-02-20]]
