# Beta Runtime Stability Prevention Implementation (2026-03-04)

## Context
- After intermittent beta runtime hangs and logout inconsistency reports, the prevention plan was to harden runtime observability, reduce polling contention, add an operator recovery path, and add automatic remediation controls.

## Implemented Controls
- Added timing instrumentation helpers in `R/timingLogging.R` and integrated structured timing events into dataset refresh/load and logout/token-revocation paths.
- Hardened dataset polling in `R/datainputTab.R`:
  - slower retry cadence while initializing
  - steady-state polling only once table discovery is ready
  - exponential backoff when table lists are unchanged
  - refresh skip/error timing markers to diagnose observer contention
- Added explicit session reset flow:
  - new hidden UI action `resetSessionUI`
  - server handler logs out, clears auth cookie state, and requests client reload
  - JS `session_reset` custom handler clears remembered token/cookie and reloads page
- Expanded remember-token revoke coverage in `R/loginServer.R` for consent-decline, logout button, JS fallback logout click, direct token revoke input, and reset-session trigger.
- Hardened deploy atomicity in `install.sh` using `flock` lock plus post-restart running-state verification.
- Added beta watchdog automation:
  - `security/beta_runtime_watchdog.sh`
  - `security/install_beta_watchdog_timer.sh`
  - default watchdog URL set to `http://127.0.0.1:23838/inst/app/` so health checks target the app route (not root directory listing)
  - restart only after consecutive checks where both high CPU and unhealthy HTTP/content checks are true.

## Validation
- Loaded package successfully with `devtools::load_all()`.
- Ran focused regression tests with zero failures:
  - `tests/testthat/test-login-security.R`
  - `tests/testthat/test-transformation-persistence-helpers.R`
- Shell syntax checks passed for `install.sh` and new watchdog scripts.
- Installed package to `archaeodashbeta`, restarted container, and verified clean server startup logs.
- Dry-ran watchdog logic with writable local state (`ARCHAEODASH_WATCHDOG_STATE_DIR=/tmp/...`) and observed healthy status (`code=200`, `unhealthy=0`) against `/inst/app/`.
- Installed systemd watchdog timer/service on host (`archaeodash-beta-watchdog.timer`) and confirmed active schedule with successful check logged to `/var/log/archaeodash/beta_watchdog.log`.
