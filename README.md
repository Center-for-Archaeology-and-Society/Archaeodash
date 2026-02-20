# NAA_analytical_dashboard
Analytical Dashboard for conducting various analyses to group and assess groupings for Neutron Activation Analytical chemical compositional data, as well as X-ray fluorescence data.

The package can be installed from github:

```
remotes::install_github("Center-for-Archaeology-and-Society/Archaeodash")
```

The primary tool is a Shiny app that is still a work in progress, but has functional tools for reading in data from csv or Excel, imputing missing data, transforming data, conducting PCA and several versions of cluster analysis, manually assigning groups, visualizing data, and exporting the results.

INAA_test.csv - test data for the scripts/app

## Automated testing

Run the package unit tests locally:

```
Rscript -e "devtools::install('.', upgrade='never', quiet=TRUE)"
Rscript -e "testthat::test_dir('tests/testthat', reporter='summary')"
```

Tests also run automatically in GitHub Actions on each push and pull request to `main` or `master` via `.github/workflows/r-tests.yml`.

## Database configuration

Do not commit real credentials to `.Renviron`. Use `.Renviron.example` as a template and set real values in deployment/runtime secrets.

## DB-backed end-to-end test

The auth+load shinytest2 e2e test is opt-in and requires a reachable database:

```bash
ARCHAEODASH_RUN_E2E=1 Rscript -e "testthat::test_file('tests/testthat/test-shinytest2-auth-and-load-e2e.R', reporter='summary')"
```
