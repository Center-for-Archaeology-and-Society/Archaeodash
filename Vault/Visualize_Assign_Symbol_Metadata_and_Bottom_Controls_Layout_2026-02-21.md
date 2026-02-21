# Visualize Assign Symbol Metadata and Bottom Controls Layout 2026-02-21

## Summary
- Added a symbol-metadata selector for the Visualize plot so marker symbols can be keyed from a chosen metadata column.
- Defaulted the symbol-metadata selection to the current group column when available.
- Reworked the below-plot controls layout into multiple rows and grouped columns to restore scanability on wide forms.

## Changes
- `R/plot.R`
  - Added `symbol_col` argument to `mainPlot()`.
  - Symbol mapping now uses `symbol_col` values (fallback: `attrGroups`) while color grouping remains driven by `attrGroups`.
  - Hover content now includes the active symbol metadata field/value.
- `R/visualizeassignTab.R`
  - Added `symbolGroupColumnUI` selector with default preference order: active input, saved value, current group column.
  - Passed `input$symbolGroupColumn` into `mainPlot(symbol_col = ...)`.
  - Stored symbol-column input in the persisted `inputList` used after group reassignment refresh.
  - Reworked `build_visualize_controls(scroll_controls = FALSE)` to use structured `fluidRow()`/`column()` groups instead of a long single stacked list.
- `tests/testthat/test-plot-mainPlot.R`
  - Added regression test that verifies metadata-driven symbol mapping emits multiple expected symbols.

## Validation
- Ran: `devtools::test(filter = 'plot-mainPlot|visualize-layout-default')`
- Result: pass (`40` tests, `0` failures).

## Related
- [[Workflows_MOC]]
- [[Interaction_Log_2026-02-21]]
