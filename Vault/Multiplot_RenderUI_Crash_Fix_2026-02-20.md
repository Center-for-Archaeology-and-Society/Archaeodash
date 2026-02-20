# Multiplot RenderUI Crash Fix 2026-02-20

## Summary
Fixed a multiplot rendering wiring bug that could destabilize the beta app session when users triggered multiplot updates.

## Root Cause
- `visualizeAssignServer` used `renderUI()` but returned `renderPlotly()`/`renderPlot()` objects instead of UI tags.
- This mixed output contract can cause invalid output behavior during reactive updates.

## Changes
- Replaced `uiOutput("multiplot")` with `uiOutput("multiplotUI")`.
- `multiplotUI` now returns only UI tags:
  - `plotlyOutput("multiplotPlotly")` for interactive mode
  - `plotOutput("multiplotStatic")` for static mode
- Added dedicated renderers:
  - `output$multiplotPlotly <- plotly::renderPlotly(...)`
  - `output$multiplotStatic <- renderPlot(...)`
- Added a modal loading indicator around multiplot generation (`Building multiplot...`) with guaranteed cleanup on success/error via `on.exit`.
- Added/extended test coverage to assert `multiplot()` returns `ggplot` (static) and `plotly` (interactive).

## Validation
- `tests/testthat/test-plot-mainPlot.R` passed.
- Full `tests/testthat` suite passed (existing CRAN-gated shinytest2 skips unchanged).
- Reinstalled package into `archaeodashbeta` and restarted only beta container.

## Related
- [[Quality_MOC]]
- [[Interaction_Log_2026-02-20]]
