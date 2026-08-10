# Registration Consent Modal Input Persistence Fix 2026-08-10

## Summary
New user registration failed because the initial login/register modal was removed before consent confirmation, so the final registration observer could lose `username`, `password`, and `email` input values.

## Fix
- Store pending registration values in `credentials$pending_registration` before replacing the modal with the consent dialog.
- Read and clear that pending registration snapshot when the user clicks `registerConfirm`.
- Harden `make_login_rate_limit_key()` for missing or empty request metadata.
- Added regression coverage for registration after the original modal inputs are cleared.

## Related
- [[Authentication_and_Cookie_Flow]]
- [[Email_Verification_and_Password_Reset_2026-05-01]]
