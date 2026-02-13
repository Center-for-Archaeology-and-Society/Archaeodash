#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$ROOT_DIR"

if ! git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
  echo "Error: not inside a git repository." >&2
  exit 1
fi

if ! git diff --quiet; then
  echo "Error: unstaged changes detected. Stage or stash them before deploy." >&2
  exit 1
fi

if [[ -n "$(git ls-files --others --exclude-standard)" ]]; then
  echo "Error: untracked files detected. Add or remove them before deploy." >&2
  exit 1
fi

BRANCH="$(git rev-parse --abbrev-ref HEAD)"
if [[ -z "$BRANCH" || "$BRANCH" == "HEAD" ]]; then
  echo "Error: detached HEAD is not supported for deploy." >&2
  exit 1
fi

if ! git rev-parse --abbrev-ref --symbolic-full-name "@{u}" >/dev/null 2>&1; then
  echo "Error: branch '$BRANCH' has no upstream configured." >&2
  exit 1
fi
UPSTREAM="$(git rev-parse --abbrev-ref --symbolic-full-name "@{u}")"
REMOTE="${UPSTREAM%%/*}"

if ! command -v R >/dev/null 2>&1; then
  echo "Error: R is not installed or not on PATH." >&2
  exit 1
fi
if ! command -v Rscript >/dev/null 2>&1; then
  echo "Error: Rscript is not installed or not on PATH." >&2
  exit 1
fi

VERSION="$(date +%Y.%m.%d.%H%M)"
TAG="v$VERSION"

if git rev-parse "$TAG" >/dev/null 2>&1; then
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

git add DESCRIPTION

if git diff --cached --quiet; then
  echo "Error: nothing staged to commit after version update." >&2
  exit 1
fi

git commit -m "Release $VERSION"
git tag "$TAG"

CRAN_REPO="${CRAN_REPO:-https://cloud.r-project.org}"
Rscript -e "
  desc <- read.dcf('DESCRIPTION')[1, ]
  fields <- c('Depends', 'Imports', 'LinkingTo', 'Suggests')
  raw <- unlist(desc[intersect(fields, colnames(read.dcf('DESCRIPTION')))])
  tokens <- trimws(unlist(strsplit(raw, ',')))
  tokens <- tokens[nzchar(tokens)]
  pkgs <- gsub('^([A-Za-z0-9.]+).*$','\\\\1', tokens)
  pkgs <- setdiff(unique(pkgs), c('R', rownames(installed.packages(priority='base'))))

  ap <- available.packages(repos='${CRAN_REPO}')
  deps <- unique(unlist(tools::package_dependencies(pkgs, db=ap, recursive=TRUE), use.names=FALSE))
  needed <- unique(c(pkgs, deps))
  installed <- rownames(installed.packages())
  missing <- setdiff(needed, installed)

  unavailable <- setdiff(missing, rownames(ap))
  if (length(unavailable) > 0) {
    stop(paste('Unresolvable dependencies:', paste(unavailable, collapse=', ')))
  }

  if (length(missing) > 0) {
    message('Installing missing dependencies: ', paste(missing, collapse=', '))
    install.packages(missing, repos='${CRAN_REPO}')
  } else {
    message('No missing dependencies.')
  }
"

R CMD INSTALL .

git push "$REMOTE" "$BRANCH"
git push "$REMOTE" "$TAG"

echo "Deployed version $VERSION on branch $BRANCH with tag $TAG."
