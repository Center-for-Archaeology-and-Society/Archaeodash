# Interaction Log 2026-08-10

- Investigated broken new user registration; fixed lost modal input state during consent confirmation and added focused auth regression coverage.
- Deployed the registration fix to the production `archaeodash` Docker Compose service and verified the app returns HTTP 200 on port 13838.
- Created and pushed branch `login_issue`, committed the current login-fix working state, ran `deploy.sh`, and verified `archaeodashbeta` returns HTTP 200 on port 23838.
- Reran `deploy.sh` with live service overrides for `archaeodash`, deployed version `2026.08.10.2011`, and verified HTTP 200 on port 13838.
- Checked live registration database path; DB connection and auth schema were healthy, but live auth email env was missing, so added a live `.env.runtime` and recreated `archaeodash` with SMTP settings.
