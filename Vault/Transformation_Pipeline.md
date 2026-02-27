# Transformation Pipeline

## Summary
Confirm action builds an analysis-ready dataset using: group filter, imputation, ratio derivation, transformation, then optional PCA/UMAP/LDA.

## Implementation
- Main flow: `run_confirmed_transformation()` in `R/datainputTab.R`
- Imputation: `mice` methods (`rf`, `pmm`, `midastouch`)
- Transform: none/log/log10/z-score path
- Derived ordinations computed in `compute_ordinations()`

## State Output
Writes `selectedData`, `chem`, ordination tables, and a named transformation snapshot.

## Related
- [[Ratio_Variables_Model]]
- [[Persistence_Transformation_Storage_Model]]
