# Edit Assignment and RowID Integrity

## Summary
Cell/group reassignment updates multiple reactive tables and keeps row-level identity stable across derived views.

## Core Functions
- Replace values across tables: `replaceCell()` in `R/editData.R`
- Enforce rowid guarantees: `ensure_rowid_column()`, `ensure_core_rowids()` in `R/rowidIntegrity.R`

## Why It Matters
Assignment workflows (plot brush, membership, Euclidean, cluster) rely on rowid joins back to canonical data.

## Related
- [[Visualize_Assign_Workflow]]
- [[Membership_Probability_Workflow]]
- [[Euclidean_Distance_Workflow]]
- [[Cluster_Workflow]]
