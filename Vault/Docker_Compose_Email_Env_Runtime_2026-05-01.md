# Docker Compose Email Env Runtime - 2026-05-01

## Context

Email configuration existed in local R runtime files, but Docker Compose did not explicitly pass those values into the `archaeodashbeta` container environment.

## Easiest Fix

- Created an ignored `.env.runtime` file from the existing local `.Renviron` email settings, normalized as `KEY=value` lines for Docker Compose.
- Added `.env.runtime` to `.gitignore`.
- Added `env_file: ./Archaeodashbeta/.env.runtime` to the Compose-managed `archaeodashbeta` service.
- Recreated only the `archaeodashbeta` service so the running container receives the environment directly from Compose.

## Verification

- Docker Compose validates the `archaeodashbeta` service.
- The recreated container is running on port `23838`.
- The container environment contains all required `ARCHAEODASH_*` email variables.
- R resolves auth email as enabled, SMTP mode, with sender, base URL, SMTP server, SMTP username, SMTP password, and `curl` available.
- A live test email to the operator address succeeded after switching the ASU SMTP transport to unauthenticated relay mode on port 25 with opportunistic TLS.

## Security Note

Do not commit `.env.runtime`; it contains deployment secret material.

## Follow-Up Change

The app email helper now supports SMTP relay mode without username/password when the SMTP server is configured and no credential variables are present.
