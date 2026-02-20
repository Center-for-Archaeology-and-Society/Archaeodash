# Interaction Log 2026-02-20

- User reported persistent Shiny render error in Visualize & Assign (UMAP + Data Ellipse + group symbols); narrowed issue to Plotly compatibility in marker symbol handling, restricted symbols to a conservative cross-version set, added explicit render error surfacing in `visualizeAssignServer`, and validated with full test suite.
- User reported the same error persisted after patch; verified container state and found `archaeodashbeta` was updated/restarted with patched package while a separate `archaeodash` container remained on an older package version, indicating likely endpoint/version mismatch during verification.
- Pulled beta Shiny logs and identified concrete failure path in `output$pointLabelColumnUI` (`if: missing value where TRUE/FALSE needed` from NA selector state); hardened selector default resolution against NA/empty values, registered plotly selection event explicitly, re-tested, reinstalled package into `archaeodashbeta`, and restarted beta container.
- User reported beta server crash during multiplot action; identified invalid output wiring (`renderUI` returning render function objects), split multiplot into proper UI + dedicated plot renderers, added regression coverage for interactive/static multiplot return types, and redeployed only `archaeodashbeta`.
- User requested a multiplot loading indicator without deployment; added a modal loading indicator (`Building multiplot...`) around multiplot generation with safe teardown and retained code-only/local-test state (no install/restart).
- User reported save-plot dialog appearing after clicking multiplot update; fixed nested observer registration by moving `savePlot` observer out of `updateMultiplot` and setting `ignoreInit=TRUE`, then validated locally without deployment.
- User requested commit/tag/restart for beta fixes; prepared release commit with vault documentation updates, created a release tag, installed current source into `archaeodashbeta`, and restarted beta container.

## Related

- [[Interaction_Log]]
- [[Visualize_Assign_UMAP_Symbol_Compatibility_Fix_2026-02-20]]
- [[Index]]
