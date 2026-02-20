# Security Audit External Attack Surface 2026-02-20

Scope: external black-box checks against `https://cas.rc.asu.edu/app/Archaeodashbeta/` plus SQL injection review of DB query construction.

## Findings
- `Medium`: `Content-Security-Policy` is `Report-Only` and includes `'unsafe-inline'` and `'unsafe-eval'`, so XSS mitigation is not actively enforced.
- `Low`: `X-Powered-By: Shiny Server` discloses backend technology.
- `Low`: SockJS discovery endpoints are exposed (`/__sockjs__/`, `/__sockjs__/info`), increasing protocol fingerprinting and potential bot/DoS targeting surface.
- `Info`: HTTP redirects to HTTPS; HSTS and core hardening headers are present; path traversal probes did not expose filesystem content.
- `Info`: No obvious SQL injection sinks found in reviewed DB code paths; sensitive queries use `DBI::sqlInterpolate`, `dbQuoteString`, and/or `dbQuoteIdentifier`.

## Evidence Snapshot
- Methods: `TRACE` returned `405`; uncommon verbs (`PUT`,`DELETE`,`PATCH`,`PROPFIND`,`OPTIONS`) returned `400`.
- Sensitive path probes (`/.git/config`, `/.env`, `/.htaccess`) returned `403`; common admin/debug probes returned `404`.
- SockJS info endpoint returned transport metadata; websocket upgrade without proper handshake failed (`400 Not a valid websocket request`).

## Recommendations
- Move CSP from report-only to enforced mode, then iteratively tighten to remove `'unsafe-eval'` and minimize `'unsafe-inline'`.
- Remove/override `X-Powered-By` response header at proxy/web tier.
- Add connection/request-rate limits specifically for SockJS/Shiny websocket paths and consider WAF rules for abnormal websocket/session churn.
- Continue parameterized SQL pattern and add explicit username/table-name normalization wherever table names are derived from user-controlled values.
