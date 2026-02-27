# Explore Tab Workflow

## Summary
Explore provides editable dataset table, crosstabs, missingness plot, histograms, and compositional profile plot.

## Key Logic
- Server: `exploreServer()` in `R/exploreTab.R`
- Crosstab helper: `compute_crosstab_summary()`
- Table edit triggers state update and autosave path via `updateCurrent()`

## Related
- [[Data_Ingestion_and_Preparation]]
