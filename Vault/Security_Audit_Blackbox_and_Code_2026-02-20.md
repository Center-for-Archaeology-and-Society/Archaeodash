# Security Audit Blackbox and Code 2026-02-20

## Scope
- External black-box checks against `https://cas.rc.asu.edu/app/Archaeodashbeta/`
- Internal code review of auth, cookie/session handling, DB access, and transport controls

## Key Findings
- Critical: remembered-login flow trusts client-supplied username cookie as authentication.
- Critical: plaintext DB credentials exist in repo-local `.Renviron`.
- High: missing baseline HTTP security headers on app responses.
- High: authentication cookie written in browser JavaScript (no `HttpOnly`, no `Secure`) and used to bootstrap session identity.
- Medium: no observable login throttling/lockout controls in auth flow.
- Medium: server technology/version headers are exposed.

## External Checks Run
- Response headers, methods, HTTP->HTTPS redirect behavior, basic traversal/probing of common sensitive paths, TLS protocol negotiation.

## Related
- [[Quality_MOC]]
- [[SQL_Injection_Audit_2026-02-18]]
- [[Authentication_and_Cookie_Flow]]
- [[Interaction_Log_2026-02-20]]
