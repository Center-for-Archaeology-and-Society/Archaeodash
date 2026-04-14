# Interaction Log 2026-04-13

- User reported multiplots loading spinner never going away and plots never loading; diagnosed three failure modes in the async `future_promise` path (missing outer tryCatch, early-return guards bypassing `clear_loader_if_active()`, promise not returned from observer), implemented targeted fixes with inner/outer tryCatch wrappers and unconditional loader clearing, added `archaeodash.multiplot.force_sync` option for testability, and added `shiny::testServer()` tests for the multiplot observer.

## Related

- [[Interaction_Log]]
- [[Multiplot_Async_Promise_Robustness_Fix_2026-04-13]]
