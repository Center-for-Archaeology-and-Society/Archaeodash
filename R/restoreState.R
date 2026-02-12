#' Restore input states
#'
#' Used to keep input states from changing when updates are made
#'
#' @param rvals reactive values object
#' @param input shiny input object
#' @param session shiny session object
#'
#' @return NULL
#'
#' @examples
#' restoreState(rvals, input, session)
restoreState <- function(rvals, input, session) {
  inputIDs <- c("attr", "attrGroups", "attrGroupsSub", "chem", "xvar", "xvar2", "yvar", "yvar2", "data.src", "Conf", "int.set", "runPCA", "runLDA", "runUMAP")
  types <- c("select", "select", "select", "select", "select", "select", "select", "select", "select", "checkbox", "slider", "checkbox", "checkbox", "checkbox")
  purrr::walk2(inputIDs, types, function(inputID, type) {
    if (isTRUE(type == "select")) {
      try(updateSelectInput(session = session, inputId = inputID, selected = rvals[[inputID]]))
    } else if (isTRUE(type == "checkbox")) {
      try(updateCheckboxInput(session = session, inputId = inputID, value = rvals[[inputID]]))
    } else if (isTRUE(type == "slider")) {
      try(updateSliderInput(session = session, inputId = inputID, value = rvals[[inputID]]))
    }
  })
}
