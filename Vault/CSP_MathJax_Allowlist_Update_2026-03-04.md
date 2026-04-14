# CSP MathJax Allowlist Update (2026-03-04)

## Context
- Browser reported CSP `Report-Only` violations for MathJax loading from:
  - `https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML`
- Active policy allowed `code.jquery.com`, `cdnjs.cloudflare.com`, and `js-de.sentry-cdn.com` but not `mathjax.rstudio.com`.

## Change
- Updated Apache SSL vhost CSP header in `/etc/apache2/sites-available/default-ssl.conf`:
  - Added `https://mathjax.rstudio.com` to both `script-src` and `script-src-elem`.
- Reloaded Apache after config test (`apache2ctl configtest`, `systemctl reload apache2`).
- Synced repository policy template in `security/apache-hardening.conf` to match allowlist behavior.

## Validation
- `curl -I https://cas.rc.asu.edu/app/Archaeodashbeta/` now returns `Content-Security-Policy-Report-Only` with `https://mathjax.rstudio.com` present in both script directives.
