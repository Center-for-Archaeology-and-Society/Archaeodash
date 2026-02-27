# Risk Reduction Plan (2026-02-18)

## Objective
Reduce regression and consistency risk across transformation and persistence flows.

## Completed
- Fixed z-score transformation function mismatch (`zScale` -> `zScore`).
- Added shared DB safety helpers in `R/dbTableOps.R`:
  - `db_table_exists_safe()`
  - `db_write_table_safe()`
  - `db_remove_table_safe()`
- Applied helpers to main dataset write/remove paths:
  - `R/DataLoader.R`
  - `R/updateCurrent.R`
  - `R/datainputTab.R`
- Added helper tests: `tests/testthat/test-db-table-ops.R`.

## Proposed Solutions (Prioritized)
1. Introduce a single persistence service module for all dataset + metadata + preference writes.
- Impact: Removes duplicated DB patterns and error handling drift.

2. Add transaction wrappers for multi-table operations (dataset + metadata + preferences).
- Impact: Prevents partial-write inconsistencies.

3. Define a minimal reactive-state contract for `rvals` (required keys, ownership, mutation boundaries).
- Impact: Reduces accidental cross-tab breakage from broad state mutation.

4. Add integration tests for persistence invariants.
- Impact: Catches schema/consistency regressions (table exists, metadata sync, rowid integrity).

## Related
- [[Key_Technical_Risks_2026-02-18]]
- [[Testing_Coverage_Map]]
- [[Shiny_State_Model_rvals]]
