# Repository Fogs, Improvements, and Security Todo - 2026-05-01

Scope: source review of the current ArchaeoDash Shiny repository, with emphasis on unclear operational assumptions ("fogs"), needed improvements, and security risks. This is an investigation todo list, not an implemented remediation set.

## Priority 0 - Fix Before Further Deployment

- [ ] Remove committed/runtime credential files from the repository tree.
  - Evidence: `.Renviron` and `inst/app/.Renviron` exist locally and contain database and/or mail configuration keys.
  - Risk: accidental commit, backup leakage, container image inclusion, or broad local read access exposes database and mail infrastructure.
  - Fix: move all real values to deployment secrets or systemd/container env, add/verify `.gitignore` entries for `.Renviron`, `inst/app/.Renviron`, `.env`, and secret variants, rotate any exposed database/mail credentials, and keep only a sanitized `.Renviron.example`.

- [ ] Replace JavaScript-managed remember-me auth cookies with server-issued `HttpOnly` cookies.
  - Evidence: `inst/app/www/app.js` sets `archaeodash_auth_token` via `document.cookie`; `R/loginServer.R` accepts `input$remembered_token` and maps it to a user.
  - Risk: any XSS or third-party script compromise can read and replay the long-lived remember token. Current hashed database storage is good, but the bearer token still lives in JavaScript-readable storage.
  - Fix: terminate auth at a proxy/server layer that can set `HttpOnly; Secure; SameSite=Lax/Strict` cookies, or add a small HTTP endpoint/middleware outside Shiny for issuing and revoking remember tokens. Shorten token lifetime until this is done.

- [x] Stop showing raw database connection errors to end users.
  - Evidence: `inst/app/server.R` includes `db_connect_error` details in a visible `showNotification`.
  - Risk: connection strings, hostnames, driver messages, table names, or infrastructure details can leak during outages.
  - Fix: show a generic user-facing message and log detailed diagnostics server-side only.

- [x] Make auth schema migrations explicit, observable, and fail-fast.
  - Evidence: `R/loginServer.R` runs `CREATE TABLE` and `ALTER TABLE` inside `try(..., silent = TRUE)` during app startup/session handling.
  - Risk: partial schema changes can silently leave login, verification, or reset flows in a broken state.
  - Fix: move auth table migrations to a versioned migration script, log each migration result, and block auth flows with a clear admin-facing error if required columns/tables are missing.

## Priority 1 - High-Value Security Hardening

- [ ] Enforce CSP instead of leaving it report-only.
  - Evidence: `security/apache-hardening.conf` sets `Content-Security-Policy-Report-Only` and allows `'unsafe-inline'` plus `'unsafe-eval'`.
  - Risk: CSP is not currently an active XSS control, which matters more because remember tokens are JavaScript-readable.
  - Fix: collect violations, move to `Content-Security-Policy`, remove unnecessary external hosts, and iteratively reduce inline/eval allowances where Shiny and Plotly permit.

- [ ] Add upload allowlisting and parser/resource limits.
  - Evidence: `inst/app/server.R` allows 100 MB requests; `R/DataLoader.R` passes uploads directly to `rio::import()` for CSV, Excel, and other supported formats.
  - Risk: broad file parsing increases denial-of-service and parser attack surface, especially with large spreadsheets or unexpected formats.
  - Fix: enforce extension/MIME allowlist, cap rows/columns/cell sizes after import, reject multi-sheet or unsupported workbook features by default, and add tests for oversized and unsupported files.

- [ ] Add persistent, cross-process rate limiting for login, registration, reset requests, and verification-email resend.
  - Evidence: login throttling is an in-memory R-process registry in `R/loginServer.R`.
  - Risk: limits reset on process restart and may not coordinate across multiple Shiny workers/containers.
  - Fix: store throttles in the database or reverse proxy, rate-limit by IP plus account/email, and add separate caps for password reset and verification email sends.

- [ ] Harden account identity rules.
  - Evidence: registration inserts username/email without obvious unique indexes for normalized email, and username validation mainly trims/lowercases.
  - Risk: duplicate emails, confusing usernames, very long usernames, or usernames that collide after cleaning can create support and data-isolation problems.
  - Fix: add explicit username length/character policy, unique normalized email constraint if one account per email is desired, and tests for collisions/case folding.

- [ ] Review all places where table names are selected from UI state before DB reads/writes.
  - Evidence: many DB accesses use selected dataset table names via `dplyr::tbl(con, tbl)` and safe wrappers. Table names are mostly constrained by per-user table discovery, but this is an important trust boundary.
  - Risk: future UI changes could allow cross-user table access or arbitrary table reads/deletes if selected table names are not revalidated immediately before use.
  - Fix: centralize `assert_user_dataset_table(con, username, table_name)` and call it before load, merge, delete, save, and transformation operations.

## Priority 2 - Reliability and Maintainability

- [ ] Split `R/datainputTab.R` into smaller modules.
  - Evidence: it owns upload, dataset discovery, loading, deletion, merge, transformation storage, variable selection, ratio variables, and action controls.
  - Risk: regressions are harder to reason about, review, and test; reactive dependencies become foggy.
  - Fix: extract upload/import, prior dataset management, transformation store UI, ratio builder, and selection-state helpers into separate files with focused tests.

- [ ] Move required runtime dependencies from `Suggests` to `Imports`.
  - Evidence: login/database/email runtime features require packages such as `DBI`, `RMySQL`, `sodium`, and `curl`, but several are listed in `Suggests`.
  - Risk: production installs can succeed while critical runtime paths fail later.
  - Fix: classify truly required runtime packages under `Imports`; keep only optional tests/dev features in `Suggests`.

- [ ] Add dependency pinning or a reproducible lockfile.
  - Evidence: `Dockerfile` and deploy scripts install packages from current CRAN/GitHub without lockfile pinning.
  - Risk: builds can change underneath the app and introduce surprise breakage or vulnerable versions.
  - Fix: adopt `renv.lock` or another pinned build strategy, rebuild images from clean state, and document the update cadence.

- [ ] Harden Docker image build hygiene.
  - Evidence: `Dockerfile` copies the entire repo into the image before install, runs as the base image default user, and does not clean apt metadata.
  - Risk: local credential files, caches, Vault notes, or test artifacts can be baked into images; larger image surface; less clear runtime permissions.
  - Fix: add `.dockerignore`, copy only needed package/app files, remove apt lists, avoid installing editor/dev tools in production images, and run as a non-root runtime user where possible.

- [ ] Add a public operational checklist for reverse-proxy hardening.
  - Evidence: `security/apache-hardening.conf` states it is not auto-applied by the R package.
  - Risk: headers and banner hardening can drift from source expectations.
  - Fix: add a validation script or documented curl checks for HSTS, CSP, X-Content-Type-Options, Referrer-Policy, Permissions-Policy, X-Frame-Options, and missing `X-Powered-By`.

## Priority 3 - Testing Gaps

- [ ] Add auth integration tests for email verification and password reset token consumption.
  - Evidence: current auth tests cover helpers and rate-limit primitives but not the full DB-backed token lifecycle.
  - Fix: use SQLite-compatible helpers or a test DB abstraction to verify issue, consume, expire, revoke, and reuse prevention.

- [ ] Add negative security tests around uploads.
  - Evidence: upload flow imports files directly and then applies transformations.
  - Fix: test unsupported extensions, oversized fixtures, empty/malformed files, many-column files, formula-like cell values, duplicate headers, and numeric coercion edge cases.

- [ ] Add authorization tests for dataset operations.
  - Evidence: dataset load/delete/merge/writeback depends on user-prefixed table discovery and selected table names.
  - Fix: test that a logged-in user cannot load, delete, merge, or autosave another user's table even if a forged input value is supplied.

- [ ] Add deployment smoke tests that validate auth, upload, dataset reload, and logout against the built container.
  - Evidence: existing tests are extensive but many browser tests skip when Chrome/Chromium is unavailable.
  - Fix: run a deterministic CI/container smoke suite with required browser dependencies installed.

## Open Fogs to Resolve

- [ ] Decide whether local uploads by unauthenticated users are intentionally supported.
  - Current behavior allows local workspace uploads without a DB save when not logged in. Confirm this is intentional and document privacy/resource implications.

- [ ] Decide the authoritative deployment model.
  - The repo includes package install scripts, a Dockerfile, Apache snippets, healthcheck timers, and direct host/container install flows. Clarify which path is production, beta, and developer-only.

- [ ] Decide whether account email can be reused.
  - Password reset uses the first user matching `LOWER(email)`, so duplicate emails could create surprising behavior unless explicitly disallowed.

- [ ] Decide how much operational metadata belongs in `Vault/` versus root docs.
  - Vault notes are excellent for internal memory, but actionable runbooks and security checklists should also live in root docs or `security/` when operators need them.

## Suggested Execution Order

1. Remove and rotate secrets, then add `.gitignore`/`.dockerignore` guardrails.
  -- ignore for now
2. Hide DB error details and make auth migrations explicit.
3. Tighten auth cookies and CSP together, because either one alone leaves a meaningful XSS/token replay gap.
4. Add upload allowlisting/resource caps.
  -- ignore for now
5. Centralize dataset-table authorization checks.
6. Rework dependency/Docker reproducibility.
7. Expand tests around auth tokens, uploads, and authorization boundaries.
