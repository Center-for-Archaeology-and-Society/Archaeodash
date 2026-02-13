resolve_group_selection <- function(all_groups, prior_selection, prior_group_column, current_group_column) {
  all_groups <- unique(as.character(all_groups))
  if (is.null(prior_selection)) prior_selection <- character()
  prior_selection <- as.character(prior_selection)
  if (length(all_groups) == 0) {
    return(character())
  }
  if (length(prior_selection) == 0 || !identical(prior_group_column, current_group_column)) {
    return(all_groups)
  }
  selection <- intersect(prior_selection, all_groups)
  if (length(selection) == 0) {
    return(all_groups)
  }
  selection
}

filter_group_choices <- function(all_groups, selection, mode = "all") {
  all_groups <- unique(as.character(all_groups))
  if (is.null(selection)) selection <- character()
  selection <- unique(as.character(selection))
  if (mode == "selected") {
    return(intersect(selection, all_groups))
  }
  if (mode == "unselected") {
    return(setdiff(all_groups, selection))
  }
  all_groups
}

merge_group_selection <- function(prior_selection, picked_values, all_groups, mode = "all") {
  if (is.null(prior_selection)) prior_selection <- character()
  if (is.null(picked_values)) picked_values <- character()
  if (is.null(all_groups)) all_groups <- character()
  prior_selection <- unique(as.character(prior_selection))
  picked_values <- unique(as.character(picked_values))
  all_groups <- unique(as.character(all_groups))
  if (mode == "unselected") {
    return(unique(union(prior_selection, picked_values)))
  }
  intersect(picked_values, all_groups)
}
