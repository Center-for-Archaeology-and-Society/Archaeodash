# NAA_analytical_dashboard
Analytical Dashboard for conducting various analyses to group and assess groupings for Neutron Activation Analytical chemical compositional data, as well as X-ray fluorescence data.

The package can be installed from github:

```
if (!require(remotes)) install.packages("remotes")
remotes::install_github("Center-for-Archaeology-and-Society/Archaeodash")
```

The primary tool is a Shiny app that is still a work in progress, but has functional tools for reading in data from csv or Excel, imputing missing data, transforming data, conducting PCA and several versions of cluster analysis, manually assigning groups, visualizing data, and exporting the results.

After the package is installed, run the following from R.

```
library(ArchaeoDash)
runArchaeoDash()
```

Use the INAA_test.csv dataset for testing which is located in inst/app, or, if accessing from the installed package
