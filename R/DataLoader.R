#' dataLoaderUI
#'
#' @return NULL
#' @export
#'
#' @examples
#' dataLoaderUI()
dataLoaderUI <- function() {
  showModal(modalDialog(
    title = "Data Loader",
    uiOutput("datasetNameUI"),
    div(textOutput("notification"), style = "color: red; font-size: 16px; padding = 10px;"),
    uiOutput("columnsUI"),
    uiOutput("loadchemUI"),
    footer = tagList(modalButton("cancel"), actionButton("loadData", "load data")),
    easyClose = F
  ))
}

#' dataLoaderServer
#'
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#'
#' @return NULL
#' @export
#'
#' @examples
#' dataLoaderServer(input, output, session, credentials, con)
dataLoaderServer <- function(input, output, session, rvals, db) {
  temp_upload <- reactiveVal()

  observeEvent(input$process_data, {
    req(input$file1)

    # Load raw data (Assumes first column is a Sample Name/ID)
    raw_df <- rio::import(input$file_upload$datapath) %>%
      mutate_all(as.character)

    temp_upload(raw_df)

    showModal(modalDialog(
      title = "Configure Dataset",
      p("Select the column that contains sample labels (e.g., ANID):"),
      selectInput("sample_label_col", "Available Columns:",
        choices = names(raw_df),
        selected = names(raw_df)[1],
        multiple = F
      ),
      p("Select the columns that contain element concentrations:"),
      checkboxGroupInput("selected_cols", "Available Columns:",
        choices = names(raw_df),
        selected = names(raw_df)
      ), # Default to all but first col
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_save", "Confirm & Save", class = "btn-success")
      )
    ))
  })

  observeEvent(input$confirm_save, {
    removeModal()
    # Generate unique IDs for this batch
    batch_ds_uuid <- UUIDgenerate()
    new_uuids <- replicate(nrow(temp_upload()), UUIDgenerate())

    # --- TABLE 1: Dataset Mapping ---
    new_datasets <- data.frame(
      sample_uuid = new_uuids,
      dataset_uuid = batch_ds_uuid,
      dataset_label = input$dataset_name
    )

    # --- TABLE 2: Labels (e.g., Original Name from file) ---
    main_label <- temp_upload() %>%
      select(any_of(input$sample_label_col)) %>%
      mutate_all(as.character) %>%
      pull()
    main_label <- data.frame(
      sample_uuid = new_uuids,
      label_type = "main",
      label = main_label
    )

    new_labels <- temp_upload() %>%
      select(-any_of(input$selected_cols)) %>%
      mutate_all(as.character) %>%
      mutate(sample_uuid = new_uuids, .before = 0) %>%
      pivot_longer(
        cols = -sample_uuid,
        names_to = "label_type",
        values_to = "label"
      )

    new_labels <- rbind(main_label, new_labels)

    data.frame(
      sample_uuid = new_uuids,
      label_type = "Original_Name",
      label = temp_upload() %>% select(-any_of(input$selected_cols))
    )

    # --- TABLE 3: Measurements (Pivoting Wide to Long) ---
    # We exclude the first column (the ID) and pivot the elements
    new_measurements <- temp_upload() %>%
      select(any_of(input$selected_cols)) %>%
      mutate_all(as.numeric) %>%
      mutate(sample_uuid = new_uuids, before = 0) %>%
      pivot_longer(
        cols = -sample_uuid,
        names_to = "measurement_label",
        values_to = "value"
      ) %>%
      mutate(measurement_type = "original")

    # Update the "Database" by appending new records
    db$datasets <- rbind(db$datasets, new_datasets)
    db$labels <- rbind(db$labels, new_labels)
    db$measurements <- rbind(db$measurements, new_measurements)
  })
}
