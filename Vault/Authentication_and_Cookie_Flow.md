# Authentication and Cookie Flow

## Summary
Login/registration is DB-backed; email verification is required before first login; password reset is email-token based; optional remembered login uses browser cookies gated by consent.

## Server
- Login/register logic: `R/loginServer.R`
- DB connection: `R/connect.R`
- Preference storage (theme, last dataset): `inst/app/server.R`, `R/datainputTab.R`

## Client
- Cookie consent + remembered user + theme storage: `inst/app/www/app.js`

## Related
- [[Email_Verification_and_Password_Reset_2026-05-01]]
- [[Persistence_Dataset_and_Metadata_Tables]]
- [[UI_Navigation_and_Layout]]
