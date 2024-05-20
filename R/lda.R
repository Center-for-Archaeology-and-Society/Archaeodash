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
getLDA = function(df = rvals$selectedData,
                  chem = rvals$chem,
                  attrGroups = rvals$attrGroups) {
  mdl = as.formula(glue::glue("`{attrGroups}` ~ ."))
  mdl_lda = tryCatch(
    MASS::lda(mdl, data = df[, c(attrGroups, chem)]),
    error = function(e) {
      mynotification(glue::glue("unable to return LDA: {e}"))
      return(NULL)
    }
  )

  if (!is.null(mdl_lda)) {
    pred_lda = predict(mdl_lda)
    coefficients <- mdl_lda$scaling
    means <- mdl_lda$means
    centered_data <-
      scale(df[, chem], center = colMeans(df[, chem]))
    discriminant_scores <-
      centered_data %*% coefficients - matrix(
        colMeans(centered_data %*% coefficients),
        ncol = ncol(coefficients),
        nrow = nrow(centered_data),
        byrow = TRUE
      )

    result = tryCatch(
      list(
        mod = mdl_lda,
        LDAdf = dplyr::bind_cols(
          df %>% dplyr::select(-tidyselect::any_of(chem)),
          discriminant_scores
        )
      ),
      error = function(e)
        return(NULL)
    )
    return(result)
  } else {
    return(NULL)
  }
}


#' plot LDA vectors
#'
#' @param lda_model lda model
#'
#' @return plot
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
  xmax = max(coefficients[, 1])
  xmin = min(coefficients[, 1])
  ymax = max(coefficients[, 2])
  ymin = min(coefficients[, 2])
  plotdf = coefficients[, c(1, 2)] %>% as.data.frame() %>% dplyr::mutate(group = rownames(coefficients))
  ggplot2::ggplot(plotdf, ggplot2::aes(x = LD1, y = LD2, label = group)) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = 0,
        y = 0,
        xend = LD1,
        yend = LD2
      ),
      lwd = 1,
      arrow = ggplot2::arrow(length = ggplot2::unit(0.4, "cm")),
      color = "#4A90E2"
    ) +
    ggplot2::geom_text(color = "#AA0317", size = 7, alpha = .6) +
    ggplot2::labs(x = "LD1", y = "LD2") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "none"
    )
}
