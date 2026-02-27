# Visualize Assign Metadata Filter NA UI Hotfix 2026-02-20

## Summary
Fixed a runtime UI error in `Visualize & Assign` metadata filtering where an `NA` filter field value triggered `if: missing value where TRUE/FALSE needed`.

## Root Cause
- `output$vizMetaFilterFieldUI` evaluated `nzchar(current)` where `current` could be `NA`.
- `output$vizMetaFilterValuesUI` assumed a valid scalar field was always present.

## Fix
- Added explicit NA/empty guards in both renderers:
  - normalize `current` before conditional checks in `vizMetaFilterFieldUI`.
  - return `NULL` early in `vizMetaFilterValuesUI` when field selection is unset/invalid.

## Validation
- Parsed and smoke-loaded `R/visualizeassignTab.R`.
- Reinstalled package into `archaeodashbeta` and restarted container.
- Confirmed no new `vizMetaFilterFieldUI` missing-value errors in recent container logs.

## Related
- [[Visualize_Assign_Right_Sidebar_Metadata_Filter_2026-02-20]]
- [[Workflows_MOC]]
- [[Interaction_Log_2026-02-20]]
