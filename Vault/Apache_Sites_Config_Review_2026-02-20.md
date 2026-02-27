# Apache Sites Config Review 2026-02-20

## Scope
- Reviewed `/etc/apache2/sites-available/000-default.conf`
- Reviewed `/etc/apache2/sites-available/default-ssl.conf`
- Reviewed effective enabled site mappings and global security conf

## Summary
- HTTPS redirect is present for target hostnames.
- SSL vhost is active and proxying ArchaeoDash and ArchaeoDashbeta paths.
- Several hardening opportunities remain (headers, server tokens/signature, directory options, request limits, proxy protections).

## Key Recommendations
- Enable strict security headers at vhost level (HSTS, CSP, frame-ancestors/X-Frame-Options, Referrer-Policy, Permissions-Policy, X-Content-Type-Options).
- Tighten global disclosure settings in `conf-enabled/security.conf` (`ServerTokens Prod`, `ServerSignature Off`).
- Remove/limit `Indexes` and broad `AllowOverride All` where not required.
- Add practical request/body/time limits and proxy timeouts for Shiny endpoints.
- Consider IP/access control for beta and admin API endpoints.
- Clean stale/broken symlinks from `sites-enabled` for removed vhosts.

## Related
- [[Security_Audit_Blackbox_and_Code_2026-02-20]]
- [[Security_Hardening_Implementation_2026-02-20]]
- [[Interaction_Log_2026-02-20]]
