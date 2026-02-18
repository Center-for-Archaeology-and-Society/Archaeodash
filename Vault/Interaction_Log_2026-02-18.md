# Interaction Log 2026-02-18

- User asked to initiate Vault role; created `Vault/` with `Index`, `Inbox`, and `Concepts`.
- User requested help-guide wording change; updated help docs to "rio package in R" with CRAN link.
- User asked why Vault lacked updates; added an atomic note for the help-guide change and linked it.
- User asked how to keep Vault updated; provided session prompt and checklist recommendation.
- User requested checklist implementation and per-interaction summaries; added `AGENTS.md` checklist and initialized this interaction log structure.
- User requested a clear vault safety rule; added explicit no-secrets/no-PII rule to `AGENTS.md`.
- User reported help navigation issue and requested Home help link; added Info/Help controls, external-link new-tab behavior, and a top-of-Home help link.
- User requested full package-to-vault buildout; analyzed Archaeodash R/Shiny codebase and added atomic MOCs/notes for architecture, workflows, persistence, testing, and risks.
- User approved quickstart addition; added `Operator_Quickstart_ArchaeoDash` and linked it from index and analysis MOC.
- User requested Mahalanobis PCA controls; added PC-count selector with PC+cumulative variance labels, wired calculation limit, added tests, installed package, and logged update note.
- User asked to address key technical risks; fixed z-score transform bug, added shared DB safe-write/remove helpers, applied them in core paths, added tests, and documented a prioritized risk-reduction plan.
- User requested commit + install for the risk-mitigation work; committed changes and reinstalled package locally.
- User requested SQL injection review and remediation; audited DB query paths, parameterized login SQL, quoted persistence identifiers, normalized merge naming, reran targeted tests, and recorded the security audit note.
- User reported missing Mahalanobis PC selector after reload; verified deployed code contained the feature, clarified selector conditions, added inline visibility hints in Group Membership UI, re-tested, reinstalled package in `archaeodashbeta`, and restarted the container.
- User requested broader PCA selector behavior; modularized PCA helper logic, enabled PC-count selection for Group Membership across methods and for Euclidean Distance, added PCA variance labels to Visualize/Assign axis dropdowns, expanded tests, installed in `archaeodashbeta`, and restarted container.
- User requested streamlined PCA labels and help; changed PC-count option labels to `First N PCs (variance%/cumulative%)`, added info-icon popovers in both Probabilities/Distances and Euclidean Distance controls, re-tested, installed to `archaeodashbeta`, and restarted container.
- User requested file-import metadata blank handling; added an optional (default-on) Data Loader setting to replace empty/NA non-element fields with `[blank]`, implemented helper-backed normalization during file import, and added tests.
- User requested release workflow; bumped package version to `2026.02.18.2319`, committed import-blank updates, tagged `v2026.02.18.2319`, and installed/restarted `archaeodashbeta`.
- User requested richer plotting controls; expanded symbol pool with repeat-on-overflow behavior, added symbol on/off toggle (default on), added point-label on/off toggle plus configurable label column (default ANID-equivalent), and persisted these settings across transformation save/load.
- User requested QA + optimization pass for plotting updates; added info popovers for symbol/label controls, removed residual debug noise, expanded docs/help text for new options, reran broader targeted tests for plot and transformation persistence paths, and validated no regressions.
- User reported selected-dataset load hang; added timeout guards around prior-dataset loading with cancel + explicit timeout notification, added timeout helper tests, then versioned, committed, tagged, and installed.

## Related

- [[Interaction_Log]]
- [[Index]]
