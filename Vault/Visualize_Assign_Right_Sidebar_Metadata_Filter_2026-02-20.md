# Visualize & Assign Right Sidebar Metadata Filter 2026-02-20

## Summary
Updated the `Visualize & Assign` tab to place controls in a right-side sidebar adjacent to the main plot and added metadata-driven visualization filtering with multi-select values.

## Problem Evaluation
- Previous layout separated plot and controls across rows, making filter/assignment workflows slower and less cohesive.
- Plot filtering by metadata was not available, which limited exploratory slicing before lasso selection.
- A naive in-place data filter would risk mutating assignment state and causing behavior regressions.

## Solution
- Reworked UI in `R/visualizeassignTab.R` so the main plot uses a `9/3` split with a scrollable right sidebar for controls.
- Added visualization-only metadata filtering controls:
  - metadata field selector (`vizMetaFilterField`)
  - metadata value multiselect (`vizMetaFilterValues`)
  - clear button (`clearVizFilter`)
- Implemented `plot_df_for_display()` reactive that filters only the rendered plot dataset by matching `rowid` from `selectedData`, preserving underlying datasets and assignment logic.
- Added selection synchronization when filter state changes so stale selected keys are dropped safely.
- Added user-facing empty-filter-state message: `No rows match the current visualization filter.`

## Why This Is Best
- Preserves existing assignment pathways while enabling flexible exploratory filtering.
- Works consistently across elements/PCA/UMAP/LDA because filtering is applied through shared `rowid`.
- Minimizes risk by isolating the filter to rendering/selection context instead of mutating canonical data reactives.

## Related
- [[Visualize_Assign_Workflow]]
- [[Workflows_MOC]]
- [[Interaction_Log_2026-02-20]]
