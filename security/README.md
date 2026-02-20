# Security Hardening (Infrastructure)

This directory contains reverse-proxy hardening snippets that cannot be enforced by Shiny application code alone.

## Files
- `apache-hardening.conf`: baseline Apache header + banner hardening for the app vhost/path.

## Notes
- Apply and validate in staging before production.
- CSP is provided in `Report-Only` mode first; tune for required Shiny/plotly resources before enforcing.
