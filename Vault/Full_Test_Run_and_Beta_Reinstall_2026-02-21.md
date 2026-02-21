# Full Test Run and Beta Reinstall 2026-02-21

## Summary
- Ran full package test suite with `devtools::test()`.
- Result: `FAIL 3 | WARN 0 | SKIP 1 | PASS 247`.
- Failures were limited to `tests/testthat/test-shinytest2-assignment-flows.R` (membership/euclidean checkbox assignment flow assertions).
- Reinstalled package into beta container and restarted service.

## Deployment
- Container: `archaeodashbeta`
- Install command: `devtools::install_local(".", force = T, dependencies = F)`
- Installed version confirmed in container: `2026.2.21.453`
- Post-restart endpoint check: `http://127.0.0.1:23838/inst/app/` returned `200`.

## Related
- [[Quality_MOC]]
- [[Interaction_Log_2026-02-21]]
