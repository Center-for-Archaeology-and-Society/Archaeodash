# Multiplot Save Dialog Observer Fix 2026-02-20

## Summary
Fixed an intermittent behavior where clicking `update` in multiplot could unexpectedly open the Save Plot dialog after users had previously downloaded a plot.

## Root Cause
- The `observeEvent(input$savePlot, ...)` handler was declared inside `observeEvent(input$updateMultiplot, ...)`.
- Each update created another save observer.
- If `savePlot` had a prior click count, newly created observers could fire immediately.

## Changes
- Moved `observeEvent(input$savePlot, ...)` out of the multiplot update observer to define it once.
- Set `ignoreInit = TRUE` on the save observer.

## Validation
- Ran `tests/testthat/test-plot-mainPlot.R` locally (pass).
- No install/restart performed in this step.

## Related
- [[Multiplot_RenderUI_Crash_Fix_2026-02-20]]
- [[Quality_MOC]]
- [[Interaction_Log_2026-02-20]]
