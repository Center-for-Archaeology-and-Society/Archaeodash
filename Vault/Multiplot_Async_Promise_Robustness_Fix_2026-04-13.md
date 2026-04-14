# Multiplot Async Promise Robustness Fix 2026-04-13

## Summary

Fixed a bug where the multiplots loading spinner never disappeared and plots never loaded.

## Root Cause

The async multiplot build path (`future::multisession` + `promises`) had three failure modes that could leave the loading modal stuck:

1. **No outer `tryCatch` around `future_promise` creation**: if `promises::future_promise()` or `promises::then()` threw a synchronous error during setup, the loading modal was shown but never closed, since `clear_loader_if_active()` was never reached.
2. **`onFulfilled` callback had an early-return guard** (`if (is_cancelled || !is_active) return(invisible(NULL))`) that bypassed `clear_loader_if_active()` in some scenarios, preventing the modal from being removed.
3. **`onRejected` callback had a similar early-return guard** that could skip `clear_loader_if_active()`, leaving the modal stuck on async worker failure.
4. **The promise was not returned from `observeEvent`**, so Shiny could not properly manage the async context for the callbacks.

## Changes

- `R/visualizeassignTab.R`
  - Added `!isTRUE(getOption("archaeodash.multiplot.force_sync", FALSE))` to `async_multiplot_enabled` to allow tests to force the synchronous path.
  - Wrapped the entire async block (`future_promise` + `then`) in an outer `tryCatch` so setup errors clear the loader and show an error notification.
  - Restructured `onFulfilled` callback: state updates (`rvals$multiplot`, `multiplot_mode`, `multiplot_height`) are guarded inside an inner `tryCatch`; `clear_loader_if_active()` is now called unconditionally after the guard (safe, because `clear_loader_if_active` itself checks the request_id before hiding the modal).
  - Restructured `onRejected` callback: notification and state reset are inside an inner `tryCatch`; `clear_loader_if_active()` is always called.
  - Returned the promise from `observeEvent` handler so Shiny has proper context for async callbacks.
- `tests/testthat/test-multiplot-server.R` (new)
  - Added `shiny::testServer()` tests for the multiplot observer in sync mode (forced via option).
  - Covers: success (ggplot output), success (plotly with interactive=TRUE), empty data (NULL output), overlapping axis selection (NULL output).

## Outcome

- Loading spinner is cleared in all scenarios: success, async worker failure, synchronous setup error, and cancelled/superseded requests.
- Plots render when data is available.
- User-visible error notification is shown on failure.
- Existing tests remain unaffected.
