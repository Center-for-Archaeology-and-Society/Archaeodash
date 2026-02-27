# Ordination Workflow

## Summary
PCA, UMAP, and LDA are computed from selected chemistry and exposed as derived data frames and plots.

## Implementation
- Server/UI: `R/ordinationTab.R`
- LDA model and validation: `R/lda.R`
- Triggered during confirm pipeline and optionally via run flags

## Related
- [[Transformation_Pipeline]]
