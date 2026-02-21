safe_scalar_chr <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[[1]])) return("")
  as.character(x[[1]])
}

available_group_assignments <- function(data, group_col) {
  if (!inherits(data, "data.frame")) return(character())
  group_col <- safe_scalar_chr(group_col)
  if (!isTRUE(nzchar(group_col))) return(character())
  if (!(group_col %in% names(data))) return(character())
  groups <- as.character(data[[group_col]])
  groups <- groups[!is.na(groups) & nzchar(groups)]
  sort(unique(groups))
}

build_group_assignment_ui <- function(choice_input_id, new_input_id, groups, selected_choice = NULL, new_label = "Enter new group designation", new_value = "") {
  groups <- as.character(groups)
  groups <- groups[!is.na(groups) & nzchar(groups)]
  groups <- sort(unique(groups))
  choices <- c(groups, "Create new group..." = "__new__")
  selected_choice <- safe_scalar_chr(selected_choice)
  new_value <- safe_scalar_chr(new_value)
  if (!isTRUE(nzchar(selected_choice)) || !isTRUE(selected_choice %in% unname(choices))) {
    selected_choice <- if (length(groups) > 0) groups[[1]] else "__new__"
  }
  shiny::tagList(
    shiny::selectInput(
      choice_input_id,
      "Assign selected to group",
      choices = choices,
      selected = selected_choice
    ),
    shiny::textInput(
      new_input_id,
      label = new_label,
      value = new_value
    )
  )
}

resolve_group_assignment_target <- function(choice_value, new_value) {
  choice_value <- safe_scalar_chr(choice_value)
  if (identical(choice_value, "__new__")) {
    return(trimws(safe_scalar_chr(new_value)))
  }
  choice_value
}

add_checkbox_column <- function(df, checked_rowids = character(), rowid_col = "rowid", checkbox_col = ".select", checkbox_class = "row-check") {
  if (!inherits(df, "data.frame")) return(df)
  if (!(rowid_col %in% names(df))) return(df)
  checked_rowids <- as.character(checked_rowids)
  checked_rowids <- checked_rowids[!is.na(checked_rowids) & nzchar(checked_rowids)]
  df %>%
    dplyr::mutate(
      !!as.name(rowid_col) := as.character(.data[[rowid_col]]),
      !!as.name(checkbox_col) := ifelse(
        .data[[rowid_col]] %in% checked_rowids,
        paste0('<input type="checkbox" class="', checkbox_class, '" data-rowid="', .data[[rowid_col]], '" checked>'),
        paste0('<input type="checkbox" class="', checkbox_class, '" data-rowid="', .data[[rowid_col]], '">')
      )
    ) %>%
    dplyr::select(tidyselect::all_of(checkbox_col), dplyr::everything())
}
