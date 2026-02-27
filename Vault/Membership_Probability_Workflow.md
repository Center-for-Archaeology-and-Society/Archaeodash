# Membership Probability Workflow

## Summary
Computes group membership via Hotelling's T2 (with Mahalanobis fallback) and supports checkbox-based reassignment.

## Implementation
- Server/UI: `R/GroupMembership.R`
- Math helpers: `R/Group_probs.R`
- Eligibility logic requires sufficient group size relative to dimensionality

## Related
- [[Edit_Assignment_and_RowID_Integrity]]
