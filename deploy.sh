#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$ROOT_DIR"

git_as_rjbischo() {
  sudo -u rjbischo git "$@"
}

if ! git_as_rjbischo rev-parse --is-inside-work-tree >/dev/null 2>&1; then
  echo "Error: not inside a git repository." >&2
  exit 1
fi

if ! git_as_rjbischo diff --quiet; then
  echo "Error: unstaged changes detected. Stage or stash them before deploy." >&2
  exit 1
fi

if [[ -n "$(git_as_rjbischo ls-files --others --exclude-standard)" ]]; then
  echo "Error: untracked files detected. Add or remove them before deploy." >&2
  exit 1
fi

BRANCH="$(git_as_rjbischo rev-parse --abbrev-ref HEAD)"
if [[ -z "$BRANCH" || "$BRANCH" == "HEAD" ]]; then
  echo "Error: detached HEAD is not supported for deploy." >&2
  exit 1
fi

if ! git_as_rjbischo rev-parse --abbrev-ref --symbolic-full-name "@{u}" >/dev/null 2>&1; then
  echo "Error: branch '$BRANCH' has no upstream configured." >&2
  exit 1
fi
UPSTREAM="$(git_as_rjbischo rev-parse --abbrev-ref --symbolic-full-name "@{u}")"
REMOTE="${UPSTREAM%%/*}"

if ! command -v R >/dev/null 2>&1; then
  echo "Error: R is not installed or not on PATH." >&2
  exit 1
fi
if ! command -v Rscript >/dev/null 2>&1; then
  echo "Error: Rscript is not installed or not on PATH." >&2
  exit 1
fi
if ! command -v docker >/dev/null 2>&1; then
  echo "Error: docker is not installed or not on PATH." >&2
  exit 1
fi
if ! docker compose version >/dev/null 2>&1; then
  echo "Error: docker compose is not available." >&2
  exit 1
fi

COMPOSE_FILE="${ARCHAEODASH_COMPOSE_FILE:-$ROOT_DIR/../docker-compose.yml}"
COMPOSE_SERVICE="${ARCHAEODASH_COMPOSE_SERVICE:-archaeodashbeta}"
CONTAINER_NAME="${ARCHAEODASH_CONTAINER:-archaeodashbeta}"

if [[ ! -f "$COMPOSE_FILE" ]]; then
  echo "Error: Docker Compose file not found: $COMPOSE_FILE" >&2
  exit 1
fi
if ! docker compose -f "$COMPOSE_FILE" config --services | grep -qx "$COMPOSE_SERVICE"; then
  echo "Error: Docker Compose service '$COMPOSE_SERVICE' not found in $COMPOSE_FILE." >&2
  exit 1
fi

VERSION="$(date +%Y.%m.%d.%H%M)"
TAG="v$VERSION"

if git_as_rjbischo rev-parse "$TAG" >/dev/null 2>&1; then
  echo "Error: git tag '$TAG' already exists." >&2
  exit 1
fi

TMP_FILE="$(mktemp)"
trap 'rm -f "$TMP_FILE"' EXIT

awk -v version="$VERSION" '
BEGIN { updated = 0 }
$1 == "Version:" {
  print "Version: " version
  updated = 1
  next
}
{ print }
END {
  if (!updated) {
    exit 2
  }
}
' DESCRIPTION > "$TMP_FILE"

mv "$TMP_FILE" DESCRIPTION
trap - EXIT

git_as_rjbischo add DESCRIPTION

if git_as_rjbischo diff --cached --quiet; then
  echo "Error: nothing staged to commit after version update." >&2
  exit 1
fi

git_as_rjbischo commit -m "Release $VERSION"
git_as_rjbischo tag "$TAG"


# Install the official uvr R companion from GitHub, then install package deps from DESCRIPTION.
Rscript -e "if (!requireNamespace('pak', quietly = TRUE)) install.packages('pak', repos = 'https://cloud.r-project.org'); if (!requireNamespace('uvr', quietly = TRUE)) pak::pak('nbafrank/uvr-r'); if (!requireNamespace('remotes', quietly = TRUE)) install.packages('remotes', repos = 'https://cloud.r-project.org'); remotes::install_deps('.')"

R CMD INSTALL .

echo "Rebuilding and recreating Docker Compose service '$COMPOSE_SERVICE'."
docker compose -f "$COMPOSE_FILE" up -d --build --force-recreate "$COMPOSE_SERVICE"

echo "Installing package inside Docker container '$CONTAINER_NAME'."
docker compose -f "$COMPOSE_FILE" exec -T -w /srv/shiny-server "$COMPOSE_SERVICE" \
  Rscript -e 'devtools::install_local(".", force = TRUE, dependencies = FALSE, upgrade = "never")'

echo "Restarting Docker Compose service '$COMPOSE_SERVICE'."
docker compose -f "$COMPOSE_FILE" restart "$COMPOSE_SERVICE"

echo "Verifying Docker container '$CONTAINER_NAME' is running."
if ! docker ps --filter "name=^${CONTAINER_NAME}$" --filter "status=running" --format '{{.Names}}' | grep -qx "$CONTAINER_NAME"; then
  echo "Error: container '$CONTAINER_NAME' is not running after deploy." >&2
  exit 1
fi

git_as_rjbischo push "$REMOTE" "$BRANCH"
git_as_rjbischo push "$REMOTE" "$TAG"

echo "Deployed version $VERSION on branch $BRANCH with tag $TAG to Docker service '$COMPOSE_SERVICE'."
