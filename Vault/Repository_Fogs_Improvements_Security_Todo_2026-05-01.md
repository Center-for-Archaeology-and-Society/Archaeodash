# Repository Fogs Improvements Security Todo 2026-05-01

Created root todo file: [[../Repository_Fogs_Improvements_Security_Todo_2026-05-01]]

## Summary
- Investigated current repository security and maintainability risks.
- Highest-priority findings: local credential files are present, remember-me auth tokens are JavaScript-readable, DB connection errors can be shown to users, auth migrations are silent, CSP remains report-only, upload parsing is broad, and Docker/dependency reproducibility needs hardening.
- No secrets were copied into Vault.

## Related
- [[Quality_MOC]]
- [[Security_Audit_Blackbox_and_Code_2026-02-20]]
- [[Security_Audit_External_Attack_Surface_2026-02-20]]
- [[Authentication_and_Cookie_Flow]]

