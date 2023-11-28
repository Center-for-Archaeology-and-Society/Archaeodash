#' Compute LDA
#'
#' @param df data frame
#' @param chem  chemical element columns
#' @param attrGroups selected attribute
#'
#' @return list of results
#' @export
#'
#' @examples
#' getLDA(df,chem,attrGroups)
getLDA = function(df = rvals$selectedData, chem = rvals$chem, attrGroups = rvals$attrGroups){

  mdl = as.formula(glue::glue("{attrGroups} ~ ."))
  mdl_lda = tryCatch(MASS::lda(mdl, data = df[,c(attrGroups,chem)]),error = function(e){
    mynotification(glue::glue("unable to return LDA: {e}"))
    return(NULL)
  })

  if(!is.null(mdl_lda)){
    pred_lda = predict(mdl_lda)
    coefficients <- mdl_lda$scaling
    means <- mdl_lda$means
    centered_data <- scale(df[, chem], center = colMeans(df[, chem]))
    discriminant_scores <- centered_data %*% coefficients - matrix(colMeans(centered_data %*% coefficients), ncol = ncol(coefficients), nrow = nrow(centered_data), byrow = TRUE)

    result = tryCatch(list(mod = mdl_lda, LDAdf = dplyr::bind_cols(df %>% dplyr::select(-tidyselect::any_of(chem)),discriminant_scores)),error = function(e)return(NULL))
    return(result)
  } else {
    return(NULL)
  }
}


#' plot LDA vectors
#'
#' @param lda_model lda model
#'
#' @return
#' @export
#'
#' @examples
#' plotLDAvectors(lda_model)
plotLDAvectors = function(lda_model) {
  # Extract coefficients
  coefficients <- lda_model$scaling
  # Set extra margin size
  extra_margin <- 1
  par(mar = c(5, 5, 2 + extra_margin, 2 + extra_margin))
  plot(1, type = "n", xlim = c(-2 - extra_margin, 2 + extra_margin), ylim = c(-2 - extra_margin, 2 + extra_margin), xlab = "LD1", ylab = "LD2")

  # Add arrows
  arrows(0, 0, coefficients[, 1], coefficients[, 2], angle = 20, length = 0.1, col = "blue")

  # Add labels for each variable
  text(coefficients[, 1], coefficients[, 2], labels = colnames(lda_model$scaling), pos = 3, col = "red")

  # Add labels for each group
  text(coefficients[, 1], coefficients[, 2], labels = rownames(coefficients), pos = 1, col = "green")
}
