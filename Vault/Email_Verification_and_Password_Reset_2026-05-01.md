# Email Verification and Password Reset 2026-05-01

## Summary
Added email-token account verification for new registrations and email-token password reset for verified users.

## Implementation
- Extended `users` with nullable `email_verified_at`.
- Added `email_verification_tokens` and `password_reset_tokens` tables alongside existing `remember_tokens`.
- Registration now creates an unverified account, sends a verification link, and does not log the user in until verification succeeds.
- Login blocks unverified accounts and resends a verification link after valid credential entry.
- Password reset requests send a time-limited reset link only for verified accounts.
- Verification and reset links are handled from query parameters in the Shiny session.

## Configuration
- Added runtime settings to `.Renviron.example` and `README.md`.
- Default delivery mode uses server `sendmail` handoff so existing system mail relay configuration can be reused without an app-level SMTP password.
- Supports `ARCHAEODASH_AUTH_EMAIL_MODE=log` for local/dev runs without real delivery, or `smtp` for direct SMTP if needed.

## Validation
- Updated login-security unit coverage for helper validation logic.
- Updated DB-backed auth shinytest2 flow to mark the test account verified before first login.

## Related
- [[Authentication_and_Cookie_Flow]]
- [[Quality_MOC]]
