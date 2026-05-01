# Deploy Script Docker Compose Beta - 2026-05-01

## Context

`deploy.sh` previously bumped `DESCRIPTION`, committed/tagged the release, installed R dependencies on the host, ran `R CMD INSTALL .`, and pushed the branch/tag. It did not deploy the updated package to the running `archaeodashbeta` Docker container.

## Change

- Added Docker and Docker Compose availability checks before release work begins.
- Defaulted deployment to `../docker-compose.yml`, service `archaeodashbeta`, and container `archaeodashbeta`.
- Added environment overrides: `ARCHAEODASH_COMPOSE_FILE`, `ARCHAEODASH_COMPOSE_SERVICE`, and `ARCHAEODASH_CONTAINER`.
- After host install succeeds, `deploy.sh` now rebuilds and recreates the Compose service, installs the package inside the running container from `/srv/shiny-server`, restarts the service, and verifies the named container is running before pushing the branch/tag.

## Verification

- `bash -n deploy.sh`
- `docker compose -f ../docker-compose.yml config --services | rg '^archaeodashbeta$'`
