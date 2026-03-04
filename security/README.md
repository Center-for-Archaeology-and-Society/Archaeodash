# Security Hardening (Infrastructure)

This directory contains reverse-proxy hardening snippets that cannot be enforced by Shiny application code alone.

## Files
- `apache-hardening.conf`: baseline Apache header + banner hardening for the app vhost/path.

## Notes
- Apply and validate in staging before production.
- CSP is provided in `Report-Only` mode first; tune for required Shiny/plotly resources before enforcing.

## Live App Health Check (Every 30 Minutes)

Install from this repository root (requires sudo):

```bash
sudo ./security/install_live_healthcheck_timer.sh
```

Optional custom URL:

```bash
sudo ./security/install_live_healthcheck_timer.sh ./security/live_healthcheck.sh https://cas.rc.asu.edu/app/Archaeodash/
```

Check timer/service status:

```bash
systemctl status archaeodash-live-healthcheck.timer --no-pager
systemctl status archaeodash-live-healthcheck.service --no-pager
```

View latest check results:

```bash
tail -n 20 /var/log/archaeodash/live_healthcheck.log
```

## Beta Runtime Watchdog (Every 2 Minutes)

Auto-restarts `archaeodashbeta` only when both conditions persist:
- high container CPU usage (default `>=85%`)
- failed HTTP/content health check for beta URL

Install from repository root (requires sudo):

```bash
sudo ./security/install_beta_watchdog_timer.sh
```

Optional custom script/URL/container:

```bash
sudo ./security/install_beta_watchdog_timer.sh ./security/beta_runtime_watchdog.sh http://127.0.0.1:23838/inst/app/ archaeodashbeta
```

Check timer/service status:

```bash
systemctl status archaeodash-beta-watchdog.timer --no-pager
systemctl status archaeodash-beta-watchdog.service --no-pager
```

View latest watchdog results:

```bash
tail -n 20 /var/log/archaeodash/beta_watchdog.log
```
