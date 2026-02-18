# Key Technical Risks (2026-02-18)

## Risk 1
Transformation path in `R/datainputTab.R` calls `zScale(transformed)` while exported helper is `zScore()` in `R/zScore.R`.

## Risk 2
Some modules still rely on implicit global-style assumptions and broad reactive mutation patterns; this increases regression risk when adding features.

## Risk 3
Database write paths are distributed across multiple modules (`DataLoader`, `updateCurrent`, `datainputTab`, transformation persistence), so consistency bugs can be subtle.

## Related
- [[Testing_Coverage_Map]]
- [[Shiny_State_Model_rvals]]
