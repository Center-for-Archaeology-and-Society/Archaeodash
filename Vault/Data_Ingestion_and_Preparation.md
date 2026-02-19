# Data Ingestion and Preparation

## Summary
Uploads are imported via `rio`, normalized, rowid-stabilized, then subset/typed for analysis.

## Pipeline
- Import: `dataLoader()` in `R/DataLoader.R`
- Dataset selection/management: `R/datainputTab.R`
- ID and chemical defaults: `default_id_column()`, `default_chem_columns()`
- Row integrity: `R/rowidIntegrity.R`

## Related
- [[Transformation_Pipeline]]
- [[Persistence_Dataset_and_Metadata_Tables]]
- [[Dataset_Selector_Refresh_and_Multiselect_2026-02-19]]
