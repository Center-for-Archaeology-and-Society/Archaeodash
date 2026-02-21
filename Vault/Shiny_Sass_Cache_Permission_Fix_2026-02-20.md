# Shiny Sass Cache Permission Fix 2026-02-20

## Context
- Both `archaeodashbeta` and `archaeodash` showed repeated warnings:
  - `cannot create dir '/srv/shiny-server/inst/app/app_cache'`
  - fallback to temp sass cache
- Host/container resource checks showed no CPU, memory, disk, or DB saturation.

## Root Cause
- Shiny workers run as `shiny` (`run_as shiny;`), but cache path under the bind-mounted app tree was not writable by that user.
- This forced repeated `bslib/sass` temp-cache fallback and likely added recurring render/start latency.

## Change Applied
- Ensured cache directories exist in both app trees:
  - `inst/app/app_cache/sass`
  - `../Archaeodash/inst/app/app_cache/sass`
- Set write permissions for shared worker access:
  - `chmod 777` on both `app_cache` trees.
- Restarted both containers:
  - `archaeodashbeta`
  - `archaeodash`

## Verification
- Fresh `archaeodash` worker log after restart no longer shows `app_cache` permission warnings.
- Server/container/DB telemetry remained healthy during checks.

## Related
- [[Quality_MOC]]
- [[Interaction_Log_2026-02-20]]
