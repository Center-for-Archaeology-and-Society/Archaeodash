# Multiplot Interactive Performance and Cancel 2026-02-20

Issue: interactive multiplot could appear to hang on large datasets due to expensive pair expansion and `ggplotly()` conversion.

## Changes
- `R/plot.R`
  - Normalized `interactive` flag parsing with logical coercion to support radio input values.
  - Reworked pair generation join to `inner_join` on `rowid` + group column and filtered same-variable pairs by variable names (`xvar != yvar`).
  - Added interactive downsampling per facet/group to cap conversion volume.
  - Applied `plotly::toWebGL()` to improve interactive rendering responsiveness.
- `R/visualizeassignTab.R`
  - Added a cancel action in the multiplot loading modal (`cancelMultiplotBuild`).
  - Added cancel-state handling to avoid publishing stale plot output after cancellation.
  - Standardized interactive-mode checks using safe logical conversion.
- `tests/testthat/test-plot-mainPlot.R`
  - Added regression test for string interactive flag handling.
  - Added regression test for same-axis pair filtering behavior.

## Outcome
- Interactive multiplot build is bounded for large workloads and less likely to stall the session.
- Users can cancel the active multiplot build request from the loading modal.
