# Interaction Log 2026-02-21

- User reported multiplot loader remaining visible and cancel button doing nothing while plots still render; diagnosed synchronous `updateMultiplot` blocking in `visualizeAssignServer`, implemented async multiplot build with request-id stale/cancel suppression, stabilized modal teardown, and validated with `devtools::test(filter = 'plot-mainPlot')`.
- User requested Visualize & Assign symbol automation and control-layout usability updates; implemented metadata-driven symbol field selection (defaulting to current group column), restored row-based grouped controls for below-plot layout, added regression coverage, and validated with `devtools::test(filter = 'plot-mainPlot|visualize-layout-default')`.
- User requested release actions for beta; bumped package version to `2026.02.21.0333`, committed `1397144`, tagged `v2026.02.21.0333`, installed missing container dependency `future`, reinstalled package into `archaeodashbeta`, and restarted beta container.
- User reported selected dataset loading still hanging and asked about test coverage; added a stalled-loader timeout regression in `test-datainput-prior-datasets.R`, hardened `inst/app/server.R` preference-table lookup with fallback naming, reapplied beta sass-cache write permissions, reinstalled package, restarted `archaeodashbeta`, and validated targeted dataset-load tests pass.
- User requested deeper import/DB-load/transformation workflow hardening; added stage-aware dataset-load failure reporting, guarded transformation load/apply with explicit catch+notifications, improved import error messaging and non-fatal preference-write handling, and validated with targeted dataset-load/dataLoader tests.
- User requested extending hardening to dataset management workflows and clearer loading-screen progress copy; added stage-aware tracing/error reporting for delete/merge/overwrite, added concise status+detail text in dataset/transformation loading modals with stage updates, validated targeted tests, and reinstalled/restarted `archaeodashbeta`.
- User reported first-group ellipse rendering red regardless of theme; fixed `mainPlot()` ellipse trace coloring to use the app group palette consistently, added regression test for marker/ellipse color consistency, validated `plot-mainPlot`, and reinstalled/restarted `archaeodashbeta`.
- User requested all pending work be committed/tagged/deployed to beta and live plus a 30-minute live health monitor; committed and tagged `v2026.02.21.0434`, installed/restarted both `archaeodashbeta` and `archaeodash`, and added `security/live_healthcheck.sh` with sudo installer `security/install_live_healthcheck_timer.sh` for a `systemd` timer.
- User requested Docker build hardening for async dependencies; confirmed `DESCRIPTION` already imports `future`, added explicit `future` and `promises` install entries in `Dockerfile` to match package imports under `dependencies = F` container installs.
- User asked whether the latest dataset-load hang root cause was identified; triaged fresh beta/live logs and runtime state, confirmed no new cache-permission signature and passing source-based dataset-load timeout tests (`devtools::test(filter='datainput-prior-datasets')`), but no single new root-cause signature yet without a timestamped reproduction window.
- User reported Shiny warning for shared input/output ID `attr`; resolved by renaming the Data Input output container from `attr` to `attrUI` while preserving input ID `attr`, then validated with `devtools::test(filter='datainput-prior-datasets')`.
- User requested local merge of remote changes; merged `origin/master` into local `master`, resolved conflicts by keeping newer local revisions in conflicted workflow/vault files, and completed merge commit `47eeeab`.
- User requested full test run then beta reinstall; ran `devtools::test()` (`FAIL 3 | WARN 0 | SKIP 1 | PASS 247`, failures in `test-shinytest2-assignment-flows.R`), reinstalled package into `archaeodashbeta`, restarted container, and verified endpoint `http://127.0.0.1:23838/inst/app/` returned `200`.
- User requested install safety on test failures and bug fixes/retest; fixed assignment-flow regressions (membership Mahalanobis fallback, DT assignment update race, group-assignment input state), added a pre-install test gate to `install.sh`, and validated `devtools::test()` now passes (`FAIL 0 | WARN 0 | SKIP 1 | PASS 263`).
- User approved release actions; committed fixes as `ae6277e`, tagged `v2026.02.21.1450`, and deployed to `archaeodashbeta` via `install.sh` with tests gated before install/restart (container tests: `FAIL 0 | WARN 0 | SKIP 4 | PASS 242`, version `2026.2.21.1450`, healthcheck `200`).

## Related

- [[Interaction_Log]]
- [[Multiplot_Async_Cancelable_Build_Fix_2026-02-21]]
- [[Visualize_Assign_Symbol_Metadata_and_Bottom_Controls_Layout_2026-02-21]]
- [[Dataset_Load_Hang_Timeout_Regression_and_Runtime_Hardening_2026-02-21]]
- [[Import_DB_Load_and_Transformation_Error_Hardening_2026-02-21]]
- [[Dataset_Management_Stage_Tracing_and_Loading_Status_Messages_2026-02-21]]
- [[Ellipse_First_Group_Red_Color_Fix_2026-02-21]]
- [[Live_Healthcheck_Timer_Setup_2026-02-21]]
- [[Docker_Build_Dependency_Hardening_Future_Promises_2026-02-21]]
- [[Local_Merge_With_Origin_Master_2026-02-21]]
- [[Full_Test_Run_and_Beta_Reinstall_2026-02-21]]
- [[Assignment_Flow_Stability_and_Install_Test_Gate_2026-02-21]]
