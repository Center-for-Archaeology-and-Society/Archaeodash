#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$ROOT_DIR"

if ! command -v Rscript >/dev/null 2>&1; then
  echo "Error: Rscript is not installed or not on PATH." >&2
  exit 1
fi

Rscript --vanilla -e "if (!requireNamespace('pak', quietly = TRUE)) install.packages('pak', repos = 'https://cloud.r-project.org'); if (!requireNamespace('uvr', quietly = TRUE)) pak::pak('nbafrank/uvr-r'); if (!requireNamespace('remotes', quietly = TRUE)) install.packages('remotes', repos = 'https://cloud.r-project.org'); remotes::install_deps('.'); devtools::load_all('.'); if (!exists('runArchaeodash', mode = 'function')) stop('runArchaeodash() was not found after devtools::load_all().'); runArchaeodash()"
