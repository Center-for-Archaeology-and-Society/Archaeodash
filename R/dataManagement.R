rvals <- list()
rvals$data <- list()
file_name <- "inst/app/INAA_test.csv"
data <- rio::import(file_name) %>%
    tibble::rowid_to_column("rowid")
names(data) %>% dput()
id_col <- "anid"
element_cols <- c(
    "as", "la", "lu", "nd", "sm", "u",
    "yb", "ce", "co", "cr", "cs", "eu", "fe", "hf", "ni", "rb", "sb",
    "sc", "sr", "ta", "tb", "th", "zn", "zr", "al", "ba", "ca", "dy",
    "k", "mn", "na", "ti", "v"
)
impute_method <- "pmm"
transform_method <- "log10"
subset <- "1.1"
label <- "CORE"

rvals <- createDataset(
    rvals = rvals,
    data = data,
    id_col = id_col,
    element_cols = element_cols,
    label = label,
    file_name
)

rvals <- modifyDataset(rvals, subset = c("1.1", "1.2"), subsetLabel = "SiteA", label_col = "CORE")

createDataset <- function(rvals, data, id_col, element_cols, label, file_name) {
    message("Creating dataset...")
    if (!inherits(data, "data.frame")) {
        stop("data must be a data.frame")
    }
    for (col in c(id_col, element_cols, label)) {
        if (!col %in% names(data)) {
            stop(paste("Column", col, "not found in data"))
        }
    }
    if (is.null(dataset_name)) {
        if (is.null(file_name)) {
            stop("Either file_name or dataset_name must be provided")
        }
        dataset_name <- tools::file_path_sans_ext(basename(file_name))
    }
    suppressWarnings({
        data <- data %>% dplyr::mutate_at(dplyr::vars(tidyselect::all_of(element_cols)), as.numeric)
    })
    dataset_index <- 1
    if (rvals$data %>% length() > 0) {
        dataset_index <- names(rvals$data) %>%
            as.numeric() %>%
            floor() %>%
            max()
        dataset_index <- dataset_index + 1
    }
    for (row in data$rowid) {
        row_df <- data %>% dplyr::filter(rowid == row)
        subset_list <- list(list(label_col = label, label = row_df[[label]]))
        names(subset_list) <- basename(file_name) %>%
            tools::file_path_sans_ext()
        rvals$data[[paste0(dataset_index, ".", row)]] <- list(
            id = row_df[[id_col]],
            id_col = id_col,
            data = row_df[, element_cols],
            metadata = row_df[, !names(row_df) %in% c(id_col, element_cols)],
            imputed_data = NULL,
            transformed_data = NULL,
            subset = list(subset_list)
        )
    }
    message("completed creating dataset")
    return(rvals)
}

modifyDataset <- function(rvals, subset, subsetLabel, label_col = "CORE") {
    message("Modifying dataset...")
    if (length(rvals$data) == 0) {
        stop("No dataset to modify")
    }
    message("subsetting")
    for (s in subset) {
        prior <- rvals$data[[s]]$subset
        new_subset <- list(list(label_col = label_col, label = subsetLabel))
        names(new_subset) <- subsetLabel
        rvals$data[[s]]$subset <- c(prior, new_subset)
    }
    message("completed modifying dataset")
    return(rvals)
}

if (isTruthy(impute_method %in% c("rf", "pmm", "midastouch"))) {
    message("imputing")
    transformed <- data[, element_cols]
    transformed <- tryCatch(mice::complete(mice::mice(transformed, method = rvals$impute.method)),
        error = function(e) {
            mynotification(e, type = "error", duration = NULL, closeButton = TRUE)
            return(NULL)
        }
    )
    if (!is.data.frame(transformed)) {
        mynotification("imputation failed", type = "error", duration = NULL, closeButton = TRUE)
    }
    message("imputed data")
}
if (isTruthy(transform_method %in% c("zscale", "log10", "log"))) {
    message("transforming")
    suppressWarnings({
        if (!inherits(transformed, "data.frame")) {
            transformed <- data[, element_cols]
        }
        if (transform_method == "zscale") {
            transformed <- zScale(transformed)
        } else if (transform_method %in% c("log10", "log")) {
            transformed <- transformed %>%
                dplyr::mutate_all(transform_method) %>%
                dplyr::mutate_all(round, digits = 3)
        }
        # get rid of infinite values
        transformed <- transformed %>%
            dplyr::mutate_all(list(function(c) {
                dplyr::case_when(!is.finite(c) ~ 0, TRUE ~ c)
            }))
        transformed <- transformed %>%
            dplyr::mutate(transformation = rvals$transform.method)
        new[, rvals$chem] <- transformed
        mynotification("transformed data")
    })
}
