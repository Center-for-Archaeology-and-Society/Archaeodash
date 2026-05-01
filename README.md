# NAA_analytical_dashboard
Analytical Dashboard for conducting various analyses to group and assess groupings for Neutron Activation Analytical chemical compositional data, as well as X-ray fluorescence data.


## Installation and Dependency Management

Install the official [uvr](https://github.com/nbafrank/uvr) R companion from GitHub, then install this package's `DESCRIPTION` dependencies:

```
Rscript -e "if (!requireNamespace('pak', quietly = TRUE)) install.packages('pak', repos = 'https://cloud.r-project.org'); if (!requireNamespace('uvr', quietly = TRUE)) pak::pak('nbafrank/uvr-r'); if (!requireNamespace('remotes', quietly = TRUE)) install.packages('remotes', repos = 'https://cloud.r-project.org'); remotes::install_deps('.')"
R CMD INSTALL .
```

This repository does not currently contain a `uvr.toml`/`uvr.lock` project, so it is not yet using `uvr sync`.

The primary tool is a Shiny app that is still a work in progress, but has functional tools for reading in data from csv or Excel, imputing missing data, transforming data, conducting PCA and several versions of cluster analysis, manually assigning groups, visualizing data, and exporting the results.

INAA_test.csv - test data for the scripts/app

## Automated testing


Run the package unit tests locally:

```
Rscript -e "if (!requireNamespace('pak', quietly = TRUE)) install.packages('pak', repos = 'https://cloud.r-project.org'); if (!requireNamespace('uvr', quietly = TRUE)) pak::pak('nbafrank/uvr-r'); if (!requireNamespace('remotes', quietly = TRUE)) install.packages('remotes', repos = 'https://cloud.r-project.org'); remotes::install_deps('.')"
Rscript -e "testthat::test_dir('tests/testthat', reporter='summary')"
```

Tests also run automatically in GitHub Actions on each push and pull request to `main` or `master` via `.github/workflows/r-tests.yml`.

## Database configuration

Do not commit real credentials to `.Renviron`. Use `.Renviron.example` as a template and set real values in deployment/runtime secrets.

For email-based account verification and password reset, set at least:

```bash
ARCHAEODASH_BASE_URL=https://your-app-host.example.com/
ARCHAEODASH_SMTP_FROM=noreply@example.com
ARCHAEODASH_SMTP_REPLY_TO=support@example.com
ARCHAEODASH_SENDMAIL_PATH=/usr/sbin/sendmail
ARCHAEODASH_AUTH_EMAIL_MODE=sendmail
```

If your server already routes mail through its default `sendmail`/`msmtp` setup, no app-level SMTP password is required.

Only set these if you explicitly want direct SMTP from the app:

```bash
ARCHAEODASH_SMTP_SERVER=smtps://smtp.example.com:465
ARCHAEODASH_SMTP_USERNAME=your-smtp-username
ARCHAEODASH_SMTP_PASSWORD=your-smtp-password
ARCHAEODASH_SMTP_USE_SSL=force
```

Optional auth email controls:

```bash
ARCHAEODASH_AUTH_EMAIL_ENABLED=1
ARCHAEODASH_AUTH_EMAIL_MODE=sendmail
```

Set `ARCHAEODASH_AUTH_EMAIL_MODE=log` to log verification/reset emails locally instead of sending them, or `smtp` to bypass the server mailer and send directly via SMTP.

## DB-backed end-to-end test

The auth+load shinytest2 e2e test is opt-in and requires a reachable database:

```bash
ARCHAEODASH_RUN_E2E=1 Rscript -e "testthat::test_file('tests/testthat/test-shinytest2-auth-and-load-e2e.R', reporter='summary')"
```
