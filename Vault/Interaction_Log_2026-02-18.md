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

## Related

- [[Interaction_Log]]
- [[Index]]
