# Auth User Duplicate Cleanup Email Verification - 2026-05-01

## Summary

Cleaned duplicate auth-account email groups and marked all remaining current users as email verified.

## Actions

- Backed up the auth tables to a root-only RDS file outside the repository before changes.
- Removed two obvious test duplicate accounts from the operator-email duplicate group.
- Preserved the real account in the second duplicate group that had dataset tables by assigning a temporary unique placeholder email pending the correct address.
- Removed auth-token rows for deleted duplicate accounts.
- Set `email_verified_at` for all remaining users.

## Verification

- Remaining users: 29.
- Unverified users: 0.
- Duplicate email groups: 0.
- The operator email now resolves to exactly one verified account.

## Follow-Up

Replace the temporary placeholder email on the preserved account once the correct address is known.
