# Mahalanobis PC Selection Update (2026-02-18)

## Summary
Added PCA dimension control for Mahalanobis membership calculations and improved PC selector labels with cumulative variance.

## What Changed
- Added a new UI selector in Group Membership when:
  - dataset = `principal components`
  - method = `Mahalanobis`
- Selector now controls how many leading PCs are used in the Mahalanobis calculation.
- Dropdown labels now include both:
  - current PC variance percent
  - cumulative variance percent through that PC

## Implementation
- `R/GroupMembership.R`
  - Added `pc_columns_sorted()`
  - Added `membership_pc_count_choices()`
  - Added `output$membershipPCCountUI`
  - Applied selected PC count before calling `group.mem.probs()`
- `tests/testthat/test-membership-pc-selection.R`
  - Added tests for PC sorting and variance/cumulative label generation

## Validation
- Parsed updated server file successfully.
- Ran tests:
  - `tests/testthat/test-membership-pc-selection.R`
  - `tests/testthat/test-group-assignment-helpers.R`
- Installed package locally with `devtools::install_local(".", force = TRUE, dependencies = FALSE)`.

## Related
- [[Membership_Probability_Workflow]]
- [[Workflows_MOC]]
- [[Interaction_Log_2026-02-18]]
