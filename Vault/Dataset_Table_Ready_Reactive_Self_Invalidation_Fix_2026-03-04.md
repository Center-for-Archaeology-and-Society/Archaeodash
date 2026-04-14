# Dataset Table Ready Reactive Self-Invalidation Fix (2026-03-04)

## Context
- Beta runtime still entered a tight refresh loop after the earlier steady-state poll interval guard.
- `docker logs` showed repeated:
  - `dataset_refresh:start reason=steady-state-sync`
  - `dataset_refresh:end ... next_ms=300000`
  at ~15-25ms cadence instead of 300000ms.

## Root Cause
- `dataInputServer()` steady-state observer depends on `dataset_table_ready()`.
- `refresh_dataset_tables()` wrote `dataset_table_ready(TRUE)` on every successful refresh, even when already `TRUE`.
- That write re-invalidated the same observer, creating immediate reruns and a loop.

## Fix
- Added guarded helpers in `R/datainputTab.R`:
  - `get_dataset_table_ready()`
  - `set_dataset_table_ready()` (writes only when value changes)
- Replaced direct writes with guarded setter at all mutation sites:
  - auth-not-ready path
  - refresh error/success paths
  - auth-state observer reset path
- Wrapped polling observer refresh calls with `shiny::isolate(...)` to avoid implicit reactive dependency capture from internal guard values.
- Added a steady-state interval guard in `refresh_dataset_tables()` so `reason == "steady-state-sync"` cannot execute faster than the configured poll interval.
- Reinstalled the package inside container (`devtools::install_local(".", force=TRUE, ...)`) because app runtime uses installed `library(ArchaeoDash)`, then restarted `archaeodashbeta`.

## Validation
- Re-ran targeted timeout helper tests successfully.
- Confirmed container returns to low idle CPU after deploy/restart.
- Observed a fresh worker log (`app-shiny-20260304-052001-42957.log`) with startup only and no rapid `dataset_refresh:start/end` churn.
- Existing unrelated test failure remained in `test-datainput-prior-datasets.R` (legacy transformation lazy-load expectation mismatch), unchanged by this fix.

## Follow-up
- Monitor new worker logs after user reconnect to confirm steady-state sync returns to minute-level cadence.
- Consider adding a unit/integration regression test that asserts no repeated steady-state refresh without timer expiry.
