# Security Hardening Implementation 2026-02-20

## Implemented
- Replaced username-based remembered login with token-based remembered login:
  - random token issuance
  - SHA-256 token hashing server-side
  - token persistence in DB with expiry/revocation
  - token rotation on successful remembered login
- Added login/registration attempt throttling with retry delay messaging.
- Hardened client cookie handling:
  - switched remembered cookie payload from username to token
  - add `Secure` flag when served over HTTPS
  - preserve `SameSite=Lax`
  - added token revocation signaling on logout/decline
- Removed plaintext repo DB credential values from `.Renviron`; added `.Renviron.example`.
- Added DB-backed shinytest2 end-to-end auth/load test scaffold using temp user + `inst/app/INAA_test.csv`.
- Added unit tests for remember token and rate-limit helpers.
- Added reverse-proxy hardening artifacts:
  - `security/apache-hardening.conf`
  - `security/README.md`

## Not Implemented in App Code (by design/constraint)
- Reverse-proxy response headers and server banner suppression cannot be fully enforced from Shiny app code.
  - Implemented as ops-applied Apache config artifact instead.
- `HttpOnly` cannot be set on cookies created by client JavaScript.
  - Full `HttpOnly` migration requires server-set cookies at proxy/app-server layer.

## Validation
- Full `tests/testthat` suite passed.
- New e2e security test added; execution is opt-in and DB-backed.

## Related
- [[Security_Audit_Blackbox_and_Code_2026-02-20]]
- [[Quality_MOC]]
- [[Interaction_Log_2026-02-20]]
