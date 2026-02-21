# R Shiny Best Practice Evaluation (2026-03-04)

## Scope
- Performed package-level quality review for Shiny best practices with `devtools::check()`, `lintr::lint_package()`, and targeted code inspection of auth/session, refresh polling, tests, package metadata, and runtime watchdog scripts.

## Highest Priority Findings
- `R CMD check` fails due test portability issues: multiple tests use source-tree-relative `source(testthat::test_path("..","..","R",...))` and `inst/app` path assumptions, which break when tests run from installed package context.
- Dependency metadata and namespace hygiene are not best practice:
  - missing DESCRIPTION declarations for used namespaces (`glue`, `stringr`, `tibble`, `tidyr`, `purrr`, `later`, `candisc`)
  - NAMESPACE imports are minimal and do not align with unqualified function use
  - DESCRIPTION license field is non-standard (`use_mit_license()` literal)
- Packaging hygiene issues:
  - `Vault/` and hidden project files are bundled into package tarball
  - very long Vault file names create non-portable tar path warnings.

## Medium Priority Findings
- Logout click currently fires both standard `logoutUI` and JS fallback `logout_click_js`, causing duplicate logout/revoke observer execution and duplicate log lines; behavior works but is redundant.
- Timing logs are enabled by default and include usernames in structured fields; operationally useful but not ideal for least-data logging posture.
- Watchdog script sources state from disk (`source "$STATE_FILE"`). Current permissions reduce risk, but explicit parsing is safer than shell `source`.

## Metrics From This Run
- `devtools::check(...)`: failed with test errors; also reported metadata/import/packaging notes/warnings.
- `lintr::lint_package()`: 3852 lints (top: `object_usage_linter`, `line_length_linter`).

## Suggested Remediation Order
1. Fix test portability for installed-package test execution (`system.file()` and helper-driven loading patterns).
2. Correct DESCRIPTION + NAMESPACE dependency declarations and regenerate docs.
3. Tighten package build hygiene (`.Rbuildignore`, exclude Vault/project artifacts from build tarball).
4. Reduce runtime log data exposure and de-duplicate logout event handling.
5. Replace watchdog `source` with explicit key parsing.
