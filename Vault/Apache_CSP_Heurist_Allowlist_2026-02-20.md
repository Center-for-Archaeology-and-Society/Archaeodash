# Apache CSP Heurist Allowlist 2026-02-20

## Problem
- Heurist pages (for example `springDiagram.php`) load external JS/CSS from `code.jquery.com`, `cdnjs.cloudflare.com`, and `js-de.sentry-cdn.com`.
- Current CSP allows only `'self'` for external script/style origins, so browser reports policy violations.

## Recommended Apache CSP Update
- Keep a strict default policy.
- Add explicit host allowlists for required `script-src` and `style-src`.
- Keep `Content-Security-Policy-Report-Only` while validating, then move to enforced `Content-Security-Policy`.

Example:

```apache
Header always set Content-Security-Policy-Report-Only "default-src 'self'; base-uri 'self'; object-src 'none'; frame-ancestors 'self'; script-src 'self' 'unsafe-inline' 'unsafe-eval' https://code.jquery.com https://cdnjs.cloudflare.com https://js-de.sentry-cdn.com; style-src 'self' 'unsafe-inline' https://code.jquery.com; img-src 'self' data: https:; font-src 'self' data: https:; connect-src 'self' https:;"
```

## Notes
- `unsafe-inline` and `unsafe-eval` are currently required by legacy dependencies; remove them after migrating to nonce/hash-based scripts and compatible libraries.
- If Sentry beacon/API calls are blocked, add the exact Sentry ingest host to `connect-src`.

## Related
- [[Apache_Sites_Config_Review_2026-02-20]]
- [[Security_Hardening_Implementation_2026-02-20]]
- [[Interaction_Log_2026-02-20]]
