# System Architecture ArchaeoDash

## Summary
ArchaeoDash is an R package with an embedded Shiny app (`inst/app/ui.R`, `inst/app/server.R`) launched via `runArchaeoDash()`.

## Structure
- Package entry: `R/runApp.R`
- Shiny UI shell: `inst/app/ui.R`
- Shiny orchestration: `inst/app/server.R`
- Feature modules: `R/*Tab.R` + server functions in same files

## Key Pattern
Shared reactive state (`rvals`) is the central data bus across all tabs.

## Related
- [[Shiny_State_Model_rvals]]
- [[UI_Navigation_and_Layout]]
- [[ArchaeoDash_Analysis_MOC]]
