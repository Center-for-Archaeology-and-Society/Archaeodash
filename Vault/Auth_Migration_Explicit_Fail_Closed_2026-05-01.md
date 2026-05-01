# Auth Migration Explicit Fail Closed 2026-05-01

## Summary
- Replaced silent auth DDL setup with explicit migration and validation helpers.
- Auth flows now use a stored schema readiness state and fail closed with a generic user-facing message when required auth tables/columns are missing.
- Database connection failures no longer expose raw connection details to browser notifications; details are limited to app logs.
- Added tests for auth migration creation and missing-schema detection.

## Related
- [[Repository_Fogs_Improvements_Security_Todo_2026-05-01]]
- [[Authentication_and_Cookie_Flow]]
- [[Quality_MOC]]
