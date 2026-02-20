# Visualize Assign Mobile Layout Width Guard Hotfix 2026-02-20

## Summary
Fixed a crash in the responsive Visualize & Assign layout initialization caused by a length-zero `output_plot_width` value.

## Root Cause
- Mobile-default observer evaluated `if (!is.finite(plot_width))` directly.
- When `session$clientData$output_plot_width` was unset, `plot_width` became `numeric(0)`, triggering `if: argument is of length zero`.

## Fix
- Added explicit length/NA/finite guard before evaluating width:
  - return early when width is empty/NA/non-finite.
  - only then coerce scalar width and apply mobile default.

## Validation
- Parsed and smoke-loaded `R/visualizeassignTab.R`.
- Reinstalled package into `archaeodashbeta` and restarted.
- Checked recent logs: no new `argument is of length zero` or `missing value where TRUE/FALSE needed` entries.

## Related
- [[Visualize_Assign_Responsive_Filter_Layout_2026-02-20]]
- [[Workflows_MOC]]
- [[Interaction_Log_2026-02-20]]
