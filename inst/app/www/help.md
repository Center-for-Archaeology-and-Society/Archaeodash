---
output: 
  html_document: 
    toc: true
    self_contained: false
    highlight: null
    theme: null
---

## About

ArchaeoDash was developed in R Shiny to facilitate the analysis of elemental data (e.g., portable X-ray fluorescence--PXRF, Neutron Activation Analysis--NAA). It has features for data manipulation, visualization, clustering, ordination (e.g., PCA, LDA), assessment of group membership and more.

This app was originally developed by Matthew Peeples and Andrew Upton and has been extensively redesigned by Robert Bischoff in consultation with Jeffrey Ferguson.

We recommend creating an account to prevent loss of data, otherwise if the app times out, reloads, or has an error then the current progress will be lost. Uploaded data will be stored in a secure database and you retain all rights to your data. You must be logged in to access your data. No guarantees are made regarding the persistence of data storage. Please back up your data regularly.

## Registration

To register, click the *LOGIN* button on the left sidebar. Check the checkbox by *Register as a new user*. Fill out the information. There are no password requirements other than not being left empty. You must agree to the notice to complete registration. Once you are registered you will be automatically logged in. If you forget your password or username, email [rbischoff\@asu.edu](mailto:rbischoff@asu.edu).

## Login

To login click the *LOGIN* button and enter your username and password.

## Import Data

Click the *BROWSE* button to upload data. CSV and XLSX are the preferred data formats, but all types supported by the R Rio package can be uploaded. If you are logged in, then all uploaded data will be stored in the database, and the data will be available to select from the *Choose dataset(s)* drop-down list. You will be asked to choose a dataset name. One will be auto-populated from the filename of the upload. It may be shortened if the name is over 50 characters. If you are not logged in then the data will be stored in temporary memory and will not be saved. Only one dataset can be used at a time if the user is not logged in. If logged in, you can select multiple datasets from the drop-down list. Press *CONFIRM DATASETS SELECTION* to load all datasets selected from the drop-down.

## Data Manager

Selected datasets can be deleted in the *Manage datasets* section and also merged or renamed. For example, you may select three datasets from the dataset drop-down and merge them under a new or existing name.

Select the column that will be used for categorization under the heading *Select descriptive/group column.*

Choose any groups you wish to include/exclude in the next drop-down.

Choose any columns containing elmeent concentrations next. Only columns containing numeric data will be available to select as an element concentration.

*Imputation Method* controls how missing values can be inferred. The underlying Mice package in R has specific requirements which may cause this to fail. Missing values will then be converted to zeroes.

*Select Transformation* controls how the element concentrations are modified. Click the *RESET ELEMENTS TO ORIGINAL* button to undo a transformation or imputation.

*PRESS TO CONFIRM SELECTIONS* must be pressed prior to most operations. Data will not be modified until this is pressed. You will be prompted to name the transformation. If no custom name is used, a default timestamp name is assigned. Reusing an existing name will overwrite that transformation.

If you are logged in, saved transformations are stored persistently for the selected dataset(s). Each saved transformation can include:

- transformed raw data
- PCA results (if selected)
- UMAP results (if selected)
- LDA results (if selected)

Use the *Transformations* section in Data Manager to load or delete saved transformations. This allows quick switching between analytical versions (for example, different group filters) without rebuilding each workflow.

The app still creates/updates the *{username}\_current* dataset after confirming selections. If you wish to save this modified dataset as a standalone dataset, select it from the dataset drop-down list and press *MERGE/RENAME SELECTED DATASETS.*

If you wish to remove all data from your temporary workspace then press *CLEAR WORKSPACE.*

The *ADD NEW COLUMN* button can be used to create new columns.

## Save Data

Data can be exported after making any modifications to the data and pressing *PRESS TO CONFIRM SELECTIONS* button. At the bottom of the *Data Manager* sidebar is an export section. Choose the type of data you wish to export, enter the filename including the extension, and press *CLICK HERE TO SAVE FILE*.

## Explore

This tab has options for exploring the selected data.

### DATASET

This tab contains a plot displaying the number of missing or zero values for each element column as well as a view of the entire selected dataset. Data can be edited here by double clicking a cell and then clicking off the cell when the data has been changed. Extensive data modification should be done in a spreadsheet editor such as Excel and then uploaded to ArchaeoDash.

### CROSSTABS

Choose the columns you wish to group data by in column 1 and column2. Select the same column in both drop-down lists if you want to view results by only one column. Then select the type of display in the last drop-down. No result or an error may be returned if the data is not the correct type.

### UNIVARIATE PLOTS

This tab shows histograms of the element data.

### COMPOSITIONAL PLOT PROFILE

This line plot shows the variation in element concentrations across the dataset.

## ORDINATION

### PCA

This tab shows information related to the PCA analysis.

### LDA

This tab shows information related to the LDA analysis. Note: LDA analysis requires three or more groups for results to be displayed.

## CLUSTER

This section will be updated soon.

## GROUP MEMBERSHIP

This section will be updated soon.

## EUCLIDEAN DISTANCE

This section will be updated soon.

## VISUALIZE & ASSIGN

This section will be updated soon.
