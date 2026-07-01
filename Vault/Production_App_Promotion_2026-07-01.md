# Production App Promotion 2026-07-01

## Summary

Promoted the latest beta release tree to the production ArchaeoDash app.

## Source

- Beta source: `Archaeodashbeta`
- Beta git revision: `9964002`
- Release tag: `v2026.05.01.0501`
- Package version: `2026.05.01.0501`

## Production Target

- Live source directory: `../Archaeodash`
- Docker Compose service: `archaeodash`
- Container: `archaeodash`
- Local health URL: `http://127.0.0.1:13838/inst/app/`

## Outcome

- Synchronized beta source into live source while preserving runtime env files and git metadata.
- Rebuilt and recreated the live Docker Compose service.
- Installed the package inside the live container from `/srv/shiny-server`.
- Restarted the live service.
- Verified live package version `2026.5.1.501`.
- Verified live endpoint returned HTTP `200`.

## Related

- [[Beta_to_Live_Release_Sync_2026-02-20]]
- [[Interaction_Log_2026-07-01]]
