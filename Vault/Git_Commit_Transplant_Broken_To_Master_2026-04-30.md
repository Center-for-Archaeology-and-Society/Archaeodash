# Git Commit Transplant Broken To Master 2026-04-30

- Request: move the last commit on `broken` to `master` without rewriting `broken`.
- Action: cherry-picked `d90b2f22f4d224903a8ce50549e841adad376bd1` onto `master`.
- Result: `master` now includes commit `2751f91` with the message `Update deploy to use uvr package`.
- Branch state: `broken` still points at `d90b2f2`, so the change now exists on both branches.
- Follow-up: the initial CRAN-based `uvr` install assumption was later corrected because upstream now installs the R companion with `pak::pak("nbafrank/uvr-r")`.
