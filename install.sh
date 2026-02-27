#!/bin/bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DEFAULT_REPO_DIR="$SCRIPT_DIR"

# Optional arg1: repo dir path. Defaults to the directory this script is in.
REPO_DIR="${1:-$DEFAULT_REPO_DIR}"
if [ ! -d "$REPO_DIR" ]; then
  echo "Error: repository directory does not exist: $REPO_DIR" >&2
  exit 1
fi

REPO_NAME="$(basename "$REPO_DIR")"
# Optional arg2: docker container name. Defaults to lowercase repo directory name.
CONTAINER_NAME="${2:-${REPO_NAME,,}}"

echo "updating repository"
echo "repository directory: $REPO_DIR"
echo "docker container: $CONTAINER_NAME"

echo "Pulling git repository"
git -C "$REPO_DIR" pull

echo "running test suite"
if ! docker exec -i -w /srv/shiny-server "$CONTAINER_NAME" R -e 'devtools::test(stop_on_failure = TRUE)'; then
  echo "Tests failed. Skipping install and container restart." >&2
  exit 1
fi

echo "installing package"
docker exec -i -w /srv/shiny-server "$CONTAINER_NAME" R -e 'devtools::install_local(".", force = T, dependencies = F)'

echo "restarting container"
docker restart "$CONTAINER_NAME"

echo "completed"
