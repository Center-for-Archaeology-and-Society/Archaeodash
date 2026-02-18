# Visualize Assign Workflow

## Summary
Interactive plotly scatter (elements/PCA/UMAP/LDA) supports lasso selection and direct group reassignment.

## Key Logic
- Server: `visualizeAssignServer()` in `R/visualizeassignTab.R`
- Plot engine: `mainPlot()` in `R/plot.R`
- Reassignment path uses `replaceCell()` with selected `rowid`
- Group symbols: optional toggle (default on) with repeating symbol map for high group counts
- Point labels: optional toggle with configurable label column (defaults to ANID-equivalent when available)

## Related
- [[Edit_Assignment_and_RowID_Integrity]]
- [[Ordination_Workflow]]
