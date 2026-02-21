# Docker Build Dependency Hardening Future Promises 2026-02-21

## Summary
- Ensured Docker image builds include async runtime dependencies required by package imports.

## Changes
- `Dockerfile`
  - Added `future` and `promises` to the `install2.r --skipinstalled` package list.
  - This avoids runtime install failures when `devtools::install_local(..., dependencies = F)` runs in containers.

## Validation
- Verified `DESCRIPTION` already lists `future` and `promises` under `Imports`.

## Related
- [[Quality_MOC]]
- [[Interaction_Log_2026-02-21]]
