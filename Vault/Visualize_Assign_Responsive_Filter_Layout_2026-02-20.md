# Visualize Assign Responsive Filter Layout 2026-02-20

## Summary
Added a layout option to place Visualize & Assign controls below the plot and defaulted this layout for mobile-width clients.

## Changes
- Replaced static visualize/select tab layout with dynamic `output$visualizeSelectLayout`.
- Added `filters_below_plot` toggle in the controls panel (`Place filters below plot`).
- Implemented two layouts:
  - Desktop/default: plot left, controls on the right.
  - Alternate: plot first, controls below.
- Added one-time mobile defaulting using `session$clientData$output_plot_width`:
  - if width `<= 768`, set `filters_below_plot = TRUE`.
  - do not repeatedly override user choice.

## Notes
- This change only affects layout and control placement; filtering, selection, and assignment logic remain unchanged.

## Related
- [[Visualize_Assign_Right_Sidebar_Metadata_Filter_2026-02-20]]
- [[Visualize_Assign_Workflow]]
- [[Workflows_MOC]]
- [[Interaction_Log_2026-02-20]]
