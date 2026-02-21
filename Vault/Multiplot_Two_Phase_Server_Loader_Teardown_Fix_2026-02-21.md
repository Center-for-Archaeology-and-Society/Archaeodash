# Multiplot Two-Phase Server Loader Teardown Fix 2026-02-21

## Summary
- Split multiplot build into two server phases so loader open/build/close are not handled in a single observer execution.
- Ensured loader teardown always executes with an explicit session-bound modal remove.
- Resolved runtime domain error when closing modal from non-reactive callback context.

## Change
- File updated: `R/visualizeassignTab.R`
  - added queued request state (`multiplot_build_request`) for phase-separated execution.
  - phase 1 (`input$updateMultiplot`): validate + show loader + queue request payload.
  - phase 2 (`observeEvent(multiplot_build_request(), ...)`): execute build in `later::later(..., delay = 0)` and always close loader in `on.exit`.
  - bound modal operations to session explicitly:
    - `showModal(..., session = session)`
    - `removeModal(session = session)`
  - used `shiny::isolate(multiplot_loading_active())` guards so loader checks are safe from both reactive and non-reactive callbacks.

## Root Cause
- Loader close path executed in callback contexts where default reactive domain was unavailable for modal APIs, causing modal teardown failure and persistent spinner state.

## Verification
- Targeted tests:
  - `devtools::test(filter='plot-mainPlot|visualize-layout-default|shinytest2-assignment-flows|shinytest2-multiplot-loader')`
  - Result: `FAIL 0 | WARN 0 | SKIP 0 | PASS 64`
- Direct success-path smoke:
  - multiplot rendered (`multiplotStatic` present),
  - loader/modal absent (`modal-open` and loading-wrap not present).

## Deployment
- Deployed to beta via gated installer after commit/tag.

## Related
- [[Multiplot_Synchronous_Loader_Hardening_2026-02-21]]
- [[Multiplot_No_X_Warning_Loader_Clear_Fix_2026-02-21]]
- [[Interaction_Log_2026-02-21]]
- [[Quality_MOC]]
