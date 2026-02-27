# Shiny State Model rvals

## Summary
`rvals` stores canonical and derived data, plus user/session analysis state.

## Core Objects
- Canonical data: `importedData`, `selectedData`
- Derived analysis data: `pcadf`, `umapdf`, `LDAdf`, `membershipProbs`, `edistance`
- Analysis config: `chem`, `attr`, `attrGroups`, `attrGroupsSub`, impute/transform flags
- Persistence keys: `currentDatasetName`, `currentDatasetKey`
- Transform snapshots: `transformations`, `activeTransformation`

## Why It Matters
All tabs mutate/read the same state; consistency is enforced using rowid helpers and `updateCurrent()`.

## Related
- [[System_Architecture_ArchaeoDash]]
- [[Edit_Assignment_and_RowID_Integrity]]
- [[Transformation_Pipeline]]
