# Uvr GitHub Install Correction 2026-04-30

- Context: the repository had switched to `uvr` installation commands that assumed `uvr` was available on CRAN.
- Upstream check: the official installation path is the GitHub-hosted R companion package `pak::pak("nbafrank/uvr-r")`, with the CLI distributed from GitHub releases.
- Repository constraint: this project does not yet have `uvr.toml` or `uvr.lock`, so it is not ready for a full `uvr sync` workflow.
- Adjustment: deployment, local run, and README instructions now install `uvr-r` from GitHub and continue to install package dependencies from `DESCRIPTION` using `remotes::install_deps('.')`.
