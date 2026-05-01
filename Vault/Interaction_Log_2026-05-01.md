# Interaction Log 2026-05-01

- Asked whether an existing password reset process exists; verified the repo has no self-service password reset flow and currently directs users to email `rbischoff@asu.edu` for assistance.
- Requested email-based password reset plus mandatory email verification for new registrations; implemented SMTP-configurable verification/reset emails, token-backed auth tables, verified-login gating, docs updates, and auth test coverage.
- Requested reuse of the server mail relay instead of app-level SMTP credentials; switched auth mail delivery to default `sendmail` handoff with `msmtp`/system mailer compatibility and updated runtime docs.
- Requested repository fog/improvement/security-risk investigation; reviewed current auth, upload, DB, deployment, and hardening surfaces and wrote [[../Repository_Fogs_Improvements_Security_Todo_2026-05-01]] plus linked Vault note.
- Requested action on DB error disclosure and auth migrations; hid raw DB connection details from users, added explicit auth migration/schema validation helpers, failed auth closed when schema is unhealthy, added migration tests, and checked off the completed todo items.
- Reported that `deploy.sh` was not deploying to the `archaeodashbeta` container; updated it to rebuild/recreate the Compose service, install inside the container, restart it, and verify it is running.
- Asked for the easiest Docker email fix; added an ignored Compose env file for beta email settings, wired it into `archaeodashbeta`, recreated the service, and verified R sees SMTP email configuration.
- Requested a test email to the operator address; initial SMTP login failed, switched config to unauthenticated ASU relay mode, updated the app helper to allow credential-free SMTP relay, recreated beta, and sent the test successfully.
