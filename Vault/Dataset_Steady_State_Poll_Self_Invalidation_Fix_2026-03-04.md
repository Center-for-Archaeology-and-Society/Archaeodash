# Dataset Steady-State Poll Self-Invalidation Fix (2026-03-04)

## Context
- Beta users reported missing visible datasets despite successful login.
- Runtime logs showed DB connectivity was healthy and `rjbischo` tables were discovered (`dataset_refresh:end ... status=ok count=5`).

## Root Cause
- The steady-state refresh observer used a `reactiveVal` (`dataset_steady_poll_ms`) both as a dependency and as a value updated inside each steady-state refresh.
- When backoff reached the max interval (`300000`), writing the same value still invalidated dependents, causing a tight refresh loop (rapid `steady-state-sync` every ~10-20ms).
- The loop consumed worker time and could starve UI responsiveness, making dataset state appear stuck/missing.

## Implementation
- Added `get_dataset_steady_poll_ms()` and `set_dataset_steady_poll_ms()` helpers in `R/datainputTab.R`.
- `set_dataset_steady_poll_ms()` only writes when the interval actually changes, preventing self-trigger invalidation loops.
- Switched steady-state timer scheduling and timing-log output to helper getters/setters.

## Validation
- Targeted tests passed:
  - `tests/testthat/test-datainput-prior-datasets.R`
  - `tests/testthat/test-dataset-load-timeout-helpers.R`
- Post-fix deployment intended to eliminate rapid `steady-state-sync` churn while preserving dataset discovery (`count=5` for `rjbischo`).
