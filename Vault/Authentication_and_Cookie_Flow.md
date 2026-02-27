# Authentication and Cookie Flow

## Summary
Login/registration is DB-backed; optional remembered login uses browser cookies gated by consent.

## Server
- Login/register logic: `R/loginServer.R`
- DB connection: `R/connect.R`
- Preference storage (theme, last dataset): `inst/app/server.R`, `R/datainputTab.R`

## Client
- Cookie consent + remembered user + theme storage: `inst/app/www/app.js`

## Related
- [[Persistence_Dataset_and_Metadata_Tables]]
- [[UI_Navigation_and_Layout]]
