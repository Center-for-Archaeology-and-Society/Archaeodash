# Persistence Dataset and Metadata Tables

## Summary
Logged-in sessions persist datasets and metadata to per-user DB tables.

## Patterns
- Dataset table: `<username>_<dataset>`
- Metadata table: `<dataset>_metadata`
- Preference table: `<username>_preferences`

## Write Paths
- Upload/load save path: `R/DataLoader.R`
- Autosave current data: `R/updateCurrent.R`
- Dataset merge/delete/load: `R/datainputTab.R`

## Related
- [[Authentication_and_Cookie_Flow]]
- [[Persistence_Transformation_Storage_Model]]
