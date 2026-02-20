# Interaction Log 2026-02-20

- User reported persistent Shiny render error in Visualize & Assign (UMAP + Data Ellipse + group symbols); narrowed issue to Plotly compatibility in marker symbol handling, restricted symbols to a conservative cross-version set, added explicit render error surfacing in `visualizeAssignServer`, and validated with full test suite.
- User reported the same error persisted after patch; verified container state and found `archaeodashbeta` was updated/restarted with patched package while a separate `archaeodash` container remained on an older package version, indicating likely endpoint/version mismatch during verification.
- Pulled beta Shiny logs and identified concrete failure path in `output$pointLabelColumnUI` (`if: missing value where TRUE/FALSE needed` from NA selector state); hardened selector default resolution against NA/empty values, registered plotly selection event explicitly, re-tested, reinstalled package into `archaeodashbeta`, and restarted beta container.
- User reported beta server crash during multiplot action; identified invalid output wiring (`renderUI` returning render function objects), split multiplot into proper UI + dedicated plot renderers, added regression coverage for interactive/static multiplot return types, and redeployed only `archaeodashbeta`.
- User requested a multiplot loading indicator without deployment; added a modal loading indicator (`Building multiplot...`) around multiplot generation with safe teardown and retained code-only/local-test state (no install/restart).
- User reported save-plot dialog appearing after clicking multiplot update; fixed nested observer registration by moving `savePlot` observer out of `updateMultiplot` and setting `ignoreInit=TRUE`, then validated locally without deployment.
- User requested commit/tag/restart for beta fixes; prepared release commit with vault documentation updates, created a release tag, installed current source into `archaeodashbeta`, and restarted beta container.
- User reported multiplot loading indicator sometimes stays visible after plot renders; hardened cleanup logic by removing `req()` short-circuit exits in loader scope and enforcing `hide_multiplot_loading()` in a `tryCatch(..., finally=...)` block, then validated locally without deployment.
- User reported loading issue persisted and requested stricter multiplot guards plus commit/install/restart; added axis validation to block update without X or Y and block overlapping X/Y selections, added unit coverage, ran full tests, then prepared deployment to beta.
- User requested thorough security audit with external testing and recommendations only; performed black-box checks against the beta URL and code-level auth/cookie/DB review, then documented prioritized security recommendations without application code changes.
- User requested double-check and implementation of security recommendations with tests first; implemented token-based remembered login, login throttling, cookie/token hardening, credential-file cleanup, added unit + opt-in DB-backed shinytest2 temp-user/INAA e2e tests, ran full tests, and documented infra-only constraints for headers/banner controls.
- User requested Apache vhost review while keeping `.Renviron` unchanged; audited current `sites-available`/enabled config and provided hardening recommendations for headers, directory policy, proxy limits, and endpoint access control without changing server config.
- User requested commit/tag/install for `archaeodashbeta`; prepared release commit for security hardening updates, created timestamp release tag, installed package into beta container, and restarted beta only.
- User requested another external-only security audit (plus SQLi sanity check); executed black-box endpoint/header/method/path/SockJS probes against beta, verified SQL parameterization patterns in DB code, and delivered prioritized attack-vector recommendations without code changes.
- User reported upload append failure due to `bind_rows()` character-vs-numeric column mismatch; added pre-append type harmonization in `merge_loaded_data()` (numeric-like to numeric, otherwise character), added regression tests, then installed and committed to beta (no tag).

## Related

- [[Interaction_Log]]
- [[Visualize_Assign_UMAP_Symbol_Compatibility_Fix_2026-02-20]]
- [[Index]]
