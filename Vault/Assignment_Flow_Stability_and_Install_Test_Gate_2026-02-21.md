# Assignment Flow Stability and Install Test Gate 2026-02-21

## Summary
- Fixed membership probability generation when default load options introduce missing values (`0 -> NA`) and Hotellings falls back to Mahalanobis.
- Fixed assignment-flow UI/test races caused by dynamic group-assignment input re-render and DataTables proxy replacement timing.
- Added a deployment guard in `install.sh` so install/restart is blocked if tests fail.

## Root Causes
- `group.mem.probs()` fallback path dropped the Mahalanobis result due handler scoping and could return `NULL`.
- Mahalanobis/best-group logic was not resilient to missing-data rows, leading to invalid membership outputs.
- `DT::replaceData()` on assignment tables could run while table bindings were being recreated, causing client-side DataTables errors.
- Group assignment UI re-render on choice input updates could clear `*GroupAssignNew` before click handlers consumed it.

## Changes
- Updated `R/Group_probs.R`:
  - hardened eligible/chem validation,
  - fixed fallback return behavior,
  - improved Mahalanobis handling for missing/degenerate rows,
  - stabilized best-group resolution for non-ideal distance rows.
- Updated `R/GroupMembership.R` and `R/EuclideanDistance.R`:
  - removed proxy `replaceData()` race path,
  - forced reactive table refresh after assignment.
- Updated `R/groupAssignmentHelpers.R`, `R/GroupMembership.R`, `R/EuclideanDistance.R`, `R/visualizeassignTab.R`:
  - preserved assignment choice/new-value state and removed avoidable re-render coupling.
- Updated `install.sh`:
  - added `devtools::test(stop_on_failure = TRUE)` gate before install/restart.

## Verification
- Targeted e2e flow test now passes:
  - `devtools::test(filter='shinytest2-assignment-flows')`
- Full suite passes:
  - `FAIL 0 | WARN 0 | SKIP 1 | PASS 263`

## Related
- [[Quality_MOC]]
- [[Interaction_Log_2026-02-21]]
