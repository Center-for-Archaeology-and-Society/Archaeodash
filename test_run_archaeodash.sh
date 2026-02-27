#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$ROOT_DIR"

if ! command -v Rscript >/dev/null 2>&1; then
  echo "Error: Rscript is not installed or not on PATH." >&2
  exit 1
fi

Rscript --vanilla -e "
  if (!requireNamespace('devtools', quietly = TRUE)) {
    stop('Package \"devtools\" is required. Install it with install.packages(\"devtools\").')
  }

  devtools::load_all('.')

  if (!exists('runArchaeodash', mode = 'function')) {
    stop('runArchaeodash() was not found after devtools::load_all().')
  }

  runArchaeodash()
"
