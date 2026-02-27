# Persistence Transformation Storage Model

## Summary
Transformations are persisted per user+dataset key using an index table and per-transformation payload tables.

## Table Model
- Index: `<username>_transformations`
- Per transformation prefix: `<username>_tx_<dataset_key>_<transformation_name>`
- Payload tables: `_selected`, `_pca`, `_umap`, `_lda`, `_meta`

## Functions
- Persist/load/delete: `R/transformationPersistence.R`
- Snapshot object creation/apply: `R/transformationStore.R`

## Related
- [[Transformation_Pipeline]]
- [[Persistence_Dataset_and_Metadata_Tables]]
