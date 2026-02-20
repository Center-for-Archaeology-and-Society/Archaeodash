# Beta to Live Release Sync 2026-02-20

## Summary
Promoted the current beta code state to live by creating a release commit/tag in `Archaeodashbeta`, then synchronizing that exact tree into `../Archaeodash` and reinstalling/restarting the live container.

## Scope
- Source repo: `Archaeodashbeta`
- Live repo: `../Archaeodash`
- Live container: `archaeodash`

## Deployment Steps
- Commit and tag release in beta repository.
- Mirror current tracked tree into `../Archaeodash`.
- Commit synchronized live repository state.
- Install package in live container with `devtools::install_local(".", force = TRUE, dependencies = FALSE)`.
- Restart live container.

## Related
- [[Quality_MOC]]
- [[Interaction_Log_2026-02-20]]
