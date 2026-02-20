# Visualize & Assign UMAP Symbol Compatibility Fix 2026-02-20

## Summary
Resolved a persistent Visualize & Assign render failure observed in UMAP mode with Data Ellipse enabled by hardening Plotly trace construction for cross-version compatibility and improving error surfacing.

## Changes
- Reduced `mainPlot()` marker symbols to a conservative set that is reliably supported across Plotly versions.
- Updated `visualizeAssignServer` plot rendering to wrap `mainPlot()` in `tryCatch`, emit a user-visible notification, and show the concrete render error message instead of only a generic Shiny error.
- Added regression test coverage ensuring `mainPlot()` uses only the conservative marker symbol set.
- Fixed NA-handling in Visualize & Assign selector UI defaults (`xvar`, `yvar`, label column) to prevent `if: missing value where TRUE/FALSE needed` when stored reactive selections contain `NA`.
- Registered `plotly_selected` on the main plot object (`source = "A"`) to align selection events with server observers.

## Validation
- Ran `tests/testthat/test-plot-mainPlot.R` (pass).
- Ran full `tests/testthat` suite (pass; existing CRAN-gated shinytest2 skips unchanged).

## Related
- [[Quality_MOC]]
- [[Visualize_Assign_Workflow]]
- [[Interaction_Log_2026-02-20]]
