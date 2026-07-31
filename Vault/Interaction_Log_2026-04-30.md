# Interaction Log 2026-04-30

- Moved the tip commit from `broken` onto `master` by cherry-picking `d90b2f2`, creating `master` commit `2751f91` and leaving `broken` unchanged.
- Corrected the `uvr` installation guidance after confirming `uvr` is not on CRAN; updated deploy/test/docs to install `nbafrank/uvr-r` from GitHub and keep dependency installs on `DESCRIPTION`.
