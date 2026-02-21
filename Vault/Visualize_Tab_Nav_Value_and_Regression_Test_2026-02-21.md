# Visualize Tab Nav Value and Regression Test 2026-02-21

## Summary
- Fixed programmatic navbar navigation for Visualize & Assign tab by assigning an explicit tab `value`.
- Added a regression check in shinytest2 assignment flow to ensure `nav = "visualizetab"` works.

## Change
- `R/visualizeassignTab.R`
  - Added `value = "visualizetab"` to top-level Visualize & Assign `tabPanel`.
- `tests/testthat/test-shinytest2-assignment-flows.R`
  - Added assertion that `app$set_inputs(nav = "visualizetab")` updates `input$nav` accordingly.

## Verification
- Ran:
  - `devtools::test(filter='shinytest2-assignment-flows|plot-mainPlot|visualize-layout-default')`
- Result:
  - `FAIL 0 | WARN 0 | SKIP 0 | PASS 59`

## Related
- [[Multiplot_Future_Multisession_Strategy_Init_Fix_2026-02-21]]
- [[Post_Fix_Sanity_Check_2026-02-21]]
- [[Quality_MOC]]
- [[Interaction_Log_2026-02-21]]
