# Key Technical Risks (2026-02-18)

## Risk 1
Transformation path in `R/datainputTab.R` calls `zScale(transformed)` while exported helper is `zScore()` in `R/zScore.R`.
Status: Mitigated on 2026-02-18 by replacing `zScale()` with `zScore()` in `R/datainputTab.R`.

## Risk 2
Some modules still rely on implicit global-style assumptions and broad reactive mutation patterns; this increases regression risk when adding features.
Status: Partially mitigated. No full refactor yet; retained as active architectural risk.

## Risk 3
Database write paths are distributed across multiple modules (`DataLoader`, `updateCurrent`, `datainputTab`, transformation persistence), so consistency bugs can be subtle.
Status: Partially mitigated on 2026-02-18 by introducing shared safe DB helpers in `R/dbTableOps.R` and applying them to core dataset write/remove paths in `R/DataLoader.R`, `R/updateCurrent.R`, and `R/datainputTab.R`.

## Next Step
- [[Risk_Reduction_Plan_2026-02-18]]

## Related
- [[Testing_Coverage_Map]]
- [[Shiny_State_Model_rvals]]
