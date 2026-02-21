# Visualize Assign Multiplot Loader And Axis Exclusion Fix 2026-02-20

## Summary
- Fixed a stuck multiplot loading modal when multiplot generation errors occur.
- Added automatic X-to-Y exclusion in multiplot selectors so X variables are removed from Y choices.

## Changes
- `R/visualizeassignTab.R`
  - Added `resolve_multiplot_y_selection()` helper to:
    - remove selected X variables from Y choices
    - preserve valid existing Y selections
    - default Y selection to all remaining choices when prior Y becomes invalid
  - Updated `yvar2` UI to use filtered choices from the helper.
  - Added observer on `input$xvar2` to immediately update `yvar2` and drop overlaps.
  - Hardened multiplot loader modal handling:
    - `show_multiplot_loading()` now clears any prior modal safely before showing.
    - `hide_multiplot_loading()` now safely handles modal removal errors.
    - `observeEvent(input$updateMultiplot, ...)` now uses `on.exit(hide_multiplot_loading(), add = TRUE)` so teardown always runs.

## Validation
- Added tests in `tests/testthat/test-plot-mainPlot.R` for `resolve_multiplot_y_selection()`.
- Ran targeted tests:
  - `devtools::test(filter = 'plot-mainPlot|visualize-layout-default')`
  - Result: pass, no failures.

## Deployment
- Installed updated package in `archaeodashbeta` and restarted container.

## Related
- [[Workflows_MOC]]
- [[Interaction_Log_2026-02-20]]
