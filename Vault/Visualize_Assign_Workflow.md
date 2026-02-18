# Visualize Assign Workflow

## Summary
Interactive plotly scatter (elements/PCA/UMAP/LDA) supports lasso selection and direct group reassignment.

## Key Logic
- Server: `visualizeAssignServer()` in `R/visualizeassignTab.R`
- Plot engine: `mainPlot()` in `R/plot.R`
- Reassignment path uses `replaceCell()` with selected `rowid`

## Related
- [[Edit_Assignment_and_RowID_Integrity]]
- [[Ordination_Workflow]]
