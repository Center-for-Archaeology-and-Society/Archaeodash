# Ellipse First Group Red Color Fix 2026-02-21

## Summary
- Fixed inconsistent ellipse line coloring where the first group could render as red regardless of selected theme/group palette.

## Root Cause
- Ellipse traces were built from `ggplot2::stat_ellipse()` and then colored from the generated `colour` column, which follows ggplot defaults.
- Point traces used app-defined palette values (`viridis` or `Dark 3`), causing mismatch between point and ellipse colors.

## Changes
- `R/plot.R`
  - Updated ellipse trace color assignment to prioritize app palette (`palette_vals[[grp]]`) for each group.
  - Added fallback to built ellipse color only if group palette lookup is unavailable.
- `tests/testthat/test-plot-mainPlot.R`
  - Added regression test that asserts marker and ellipse colors match for common groups.

## Validation
- Ran: `devtools::test(filter = 'plot-mainPlot')`
- Result: pass (`34` tests, `0` failures).

## Related
- [[Workflows_MOC]]
- [[Interaction_Log_2026-02-21]]
