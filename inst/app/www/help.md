---
title: "ArchaeoDash Help Guide"
output: 
  html_document: 
    toc: true
    toc_float: true
    self_contained: false
    highlight: null
    theme: flatly
---

## About

ArchaeoDash was developed in R Shiny to facilitate the analysis of elemental data (for example portable X-ray fluorescence (PXRF) and Neutron Activation Analysis (NAA)). It includes tools for data management, ordination, clustering, distance-based matching, and group assignment.

This app was originally developed by Matthew Peeples and Andrew Upton and has been extensively redesigned by Robert Bischoff in consultation with Jeffrey Ferguson.

We recommend creating an account to reduce data loss risk. If the app times out, reloads, or encounters an error while not logged in, temporary work may be lost.

## Accounts and Sessions

### Registration

Click *LOGIN* in the side panel, check *Register as a new user*, and complete the form. You must accept the notice to complete registration. If you forget your password or username, email [rbischoff\@asu.edu](mailto:rbischoff@asu.edu).

### Login

Click *LOGIN* and enter your username and password.

### Cookies and Session Persistence

If cookies are accepted, ArchaeoDash stores a local login cookie to keep you signed in for up to 30 days. You can decline cookies or log out to clear the local session cookie.

## Import and Manage Data

### Import Data

Click *BROWSE* to upload data. CSV and XLSX are preferred, and other formats supported by the `rio` package may also work.

If you are logged in:

- Uploaded datasets are stored in your account database area.
- Select a dataset from *Choose dataset*.
- The selector defaults to your last opened dataset.
- Click *Confirm dataset selection* to load that dataset.

If you are not logged in:

- Data is stored in temporary session memory only.
- Only one dataset can be used at a time.

## Data Manager

### Dataset Management

Use *Manage datasets* to delete selected datasets or merge/rename selected datasets.

### Data Selection and Preparation

- Select the descriptive/group column.
- Choose groups to include.
- Select element concentration columns (numeric columns only).
- Choose an imputation method if needed.
- Choose a transformation method if needed.

Click *Press to confirm selections* to apply these choices. Most analyses require this step before they update. A loading indicator is shown while the update runs.

### Transformations

When you confirm selections, you are prompted to name the transformation:

- If left blank, a timestamp-based default name is used.
- Reusing an existing name overwrites that transformation.

Each saved transformation can include:

- Transformed selected data
- PCA results (if selected)
- UMAP results (if selected)
- LDA results (if selected)

Transformation behavior:

- Selecting a transformation in the *Transformations* dropdown loads it immediately.
- *Delete* removes the selected transformation.
- If logged in, transformations are persisted per selected dataset and can be reloaded later.

If *Run LDA* is selected with fewer than three groups, the app shows a warning because LDA visualization requires at least three groups.

### Additional Data Manager Tools

- *Reset elements to original* resets element values to original imported values.
- *Add new column* creates a new column with a default value.
- *Clear workspace* reloads the app session and clears temporary state.

## Save Data

Use the export section to download current data products. In most cases, confirm selections first so exported tables reflect current settings.

## Explore

This tab has options for exploring the selected data.

### DATASET

This tab contains a plot displaying the number of missing or zero values for each element column as well as a view of the entire selected dataset. Data can be edited here by double clicking a cell and then clicking off the cell when the data has been changed. Extensive data modification should be done in a spreadsheet editor such as Excel and then uploaded to ArchaeoDash.

When logged in, edits autosave directly to the currently selected dataset in the database.

Warning: editing metadata columns (for example group or descriptive columns) changes database records. Make sure you maintain backups before making bulk edits.

### CROSSTABS

Choose the columns you wish to group data by in column 1 and column2. Select the same column in both drop-down lists if you want to view results by only one column. Then select the type of display in the last drop-down. No result or an error may be returned if the data is not the correct type.

### UNIVARIATE PLOTS

This tab shows histograms of the element data.

### COMPOSITIONAL PLOT PROFILE

This line plot shows the variation in element concentrations across the dataset.

## VISUALIZE & ASSIGN

Use this tab to visualize data (elements, PCA, UMAP, LDA) and reassign selected points.

In *visualize and select*:

1. Choose data source and x/y variables.
2. Select points using lasso/box select.
3. Enter a new group designation.
4. Click *Change Group Assignment*.

In *multiplots*:

- Build multiple pair plots.
- Toggle interactive mode.
- Save plots to file.

## ORDINATION

### PCA

Shows PCA outputs for the current confirmed transformation.

### LDA

Shows LDA outputs for the current confirmed transformation. LDA requires at least three groups.

## CLUSTER

Cluster methods available:

- Optimal cluster count diagnostics (elbow and silhouette)
- Hierarchical agglomerative clustering
- Hierarchical divisive clustering
- K-means
- K-medoids

Cluster analysis can be run using:

- elements
- principal components (PCA)
- UMAP
- linear discriminants (LDA)

After running a method, use *Record cluster assignments* to add cluster labels back into the dataset. You can choose the output column name. If the column exists, you will be prompted to overwrite it.

## PROBABILITIES AND DISTANCES

This tab includes group size summaries and a full membership probabilities table.

### Membership Probabilities Workflow

1. Choose eligible groups and a unique sample ID column.
2. Choose method and dataset source (`elements`, `principal components`, `UMAP`, or `linear discriminants`).
3. Click *Calculate*.

The membership table supports filtering and column visibility control.

### Updating Group Assignments from Membership Probabilities

You can update group assignments directly from selected table rows:

1. Select one or more rows in the membership probabilities table.
2. Click *Assign Best Group* to set each selected row to its `BestGroup`.
3. Or enter a value in *Enter new group designation* and click *Change Group Assignment*.

These updates write back to the dataset and carry into downstream analyses and saved transformations.

## EUCLIDEAN DISTANCE

Use this tab to identify closest matches by Euclidean distance.

Workflow:

1. Choose dataset source (`elements`, `principal components`, `UMAP`, or `linear discriminants`).
2. Choose groups to project against.
3. Choose sample ID and whether to project within group.
4. Choose the number of nearest matches to return.
5. Click *Calculate*.

The results table supports reassignment:

- *Assign Match Group* sets selected rows to the matched group.
- *Change Group Assignment* applies your entered group value to selected rows.

## Notes and Troubleshooting

- If database connection fails, a persistent in-app warning is shown and account-linked storage features may be unavailable.
- If an analysis returns no results, verify that required inputs are selected and that *Press to confirm selections* has been run after recent data or option changes.
