# Interaction Log 2026-05-01

- Asked whether an existing password reset process exists; verified the repo has no self-service password reset flow and currently directs users to email `rbischoff@asu.edu` for assistance.
- Requested email-based password reset plus mandatory email verification for new registrations; implemented SMTP-configurable verification/reset emails, token-backed auth tables, verified-login gating, docs updates, and auth test coverage.
- Requested reuse of the server mail relay instead of app-level SMTP credentials; switched auth mail delivery to default `sendmail` handoff with `msmtp`/system mailer compatibility and updated runtime docs.
