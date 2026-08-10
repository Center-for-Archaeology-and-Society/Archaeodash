# Multiplot Async Cancelable Build Fix 2026-02-21

## Summary
- Reworked multiplot build flow to run asynchronously so the Shiny session can process cancel clicks while a build is running.
- Added request-id tracking so stale or canceled multiplot results are ignored.
- Decoupled multiplot render outputs from `updateMultiplot` observer redefinition by using stable reactive state (`multiplot_mode`, `multiplot_height`, `rvals$multiplot`).

## Changes
- `R/visualizeassignTab.R`
  - Added async capability detection (`promises` + `future`) and one-time `future::multisession` setup for multiplot builds.
  - Added request lifecycle state: `multiplot_request_counter`, `multiplot_active_request_id`, `multiplot_cancelled_request_id`.
  - Updated cancel handler to mark active request canceled and close loader immediately.
  - Replaced synchronous `updateMultiplot` body with async `promises::future_promise()` build path and stale/cancel guards in fulfilled/rejected callbacks.
  - Kept synchronous fallback path when async packages are unavailable.
- `DESCRIPTION`
  - Added `future` and `promises` to `Imports`.

## Outcome
- Loader no longer depends on a blocking observer completion path.
- Cancel responds during build by closing the loader and preventing stale result publication.
- Interactive and static multiplot output selection remains consistent using captured request state.

## Validation
- Ran: `devtools::test(filter = 'plot-mainPlot')`
- Result: pass (`31` tests, `0` failures).

## Related
- [[Workflows_MOC]]
- [[Interaction_Log_2026-02-21]]
