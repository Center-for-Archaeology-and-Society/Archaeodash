# Operator Quickstart ArchaeoDash

## 1) Start App
- Run `ArchaeoDash::runArchaeoDash()` from R.
- UI entrypoints: `inst/app/ui.R`, `inst/app/server.R`.

## 2) Core Mental Model
- `rvals` is the shared state bus.
- Canonical tables: `importedData` and `selectedData`.
- Most tabs read/write these and derived tables (`pcadf`, `umapdf`, `LDAdf`, `membershipProbs`, `edistance`).

## 3) Normal User Flow
- Upload/select dataset in Data Manager.
- Set grouping + element columns.
- Click `Press to confirm selections`.
- This runs: filter -> impute -> ratio derivation -> transform -> optional PCA/UMAP/LDA.

## 4) Where to Debug Fast
- Data prep pipeline: `R/datainputTab.R`.
- Assignment propagation: `R/editData.R` + `R/rowidIntegrity.R`.
- Visualization selection/assignment: `R/visualizeassignTab.R` + `R/plot.R`.
- Membership/ED calculations: `R/GroupMembership.R`, `R/Group_probs.R`, `R/EuclideanDistance.R`.

## 5) Persistence Model
- Dataset tables: `<username>_<dataset>` + `<dataset>_metadata`.
- Preferences: `<username>_preferences`.
- Transform snapshots in DB: see `R/transformationPersistence.R`.

## 6) High-Value Checks Before Release
- Run `tests/testthat`.
- Smoke test upload -> confirm selections -> each major tab.
- Verify reassignment writes back correctly (plot brush, membership, ED, cluster).

## Related
- [[ArchaeoDash_Analysis_MOC]]
- [[System_Architecture_ArchaeoDash]]
- [[Shiny_State_Model_rvals]]
- [[Persistence_MOC]]
- [[Quality_MOC]]
