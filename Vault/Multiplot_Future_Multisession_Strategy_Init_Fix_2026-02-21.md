# Multiplot Future Multisession Strategy Init Fix 2026-02-21

## Summary
- Fixed multiplot async initialization error:
  - `Unable to build multiplot: INTERNAL ERROR: The future::multisession() must never be called directly`
- Updated async plan initialization to use `future::plan(strategy = "multisession", workers = 1)`.
- Added safe fallback: if plan initialization fails, async multiplot is disabled for the session and sync plotting path is used.

## Root Cause
- Async initialization used a direct function strategy form that can trigger newer `future` package guardrails around direct `multisession()` calls.

## Change
- File updated: `R/visualizeassignTab.R`
  - Replaced plan init with strategy-string initialization.
  - Wrapped init in `tryCatch` and disabled async mode on failure instead of surfacing a hard build error.

## Verification
- Ran targeted tests:
  - `devtools::test(filter='plot-mainPlot|visualize-layout-default')`
  - Result: `FAIL 0 | WARN 0 | SKIP 0 | PASS 42`

## Related
- [[Multiplot_Async_Cancelable_Build_Fix_2026-02-21]]
- [[Quality_MOC]]
- [[Interaction_Log_2026-02-21]]
