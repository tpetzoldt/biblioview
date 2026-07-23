#' Create Standard Biblioview DataTable
#'
#' Handles hyperlink formatting, JS tooltips, column truncation,
#' and export button configurations uniformly.
#'
#' @param df Data frame containing reference entries
#' @param title Character. Export filename prefix
#' @param show_buttons Logical. Include Copy/CSV/Excel export buttons
#' @export
render_biblioview_table <- function(df, title = "export", show_buttons = TRUE) {
  clean_filename <- gsub("[^a-zA-Z0-9_-]", "_", title)
  formatted_df   <- biblioview::format_hyperlinks(df)

  abstract_col_idx <- which(tolower(names(formatted_df)) == "abstract")
  note_col_idx     <- which(tolower(names(formatted_df)) %in% c("note", "extra_note"))

  col_definitions <- list()

  # JS Truncation for Abstracts
  if (length(abstract_col_idx) > 0 && !is.na(abstract_col_idx)) {
    col_definitions[[length(col_definitions) + 1]] <- list(
      targets = abstract_col_idx,
      render = DT::JS(
        "function(data, type, row) {",
        "  if (type === 'display' && data !== null && data.length > 90) {",
        "    var cleanText = data.replace(/\"/g, '&quot;').replace(/\\n/g, ' ');",
        "    return '<span title=\"' + cleanText + '\">' + data.substring(0, 90) + '...</span>';",
        "  }",
        "  return data;",
        "}"
      )
    )
  }

  # JS Truncation for Notes
  if (length(note_col_idx) > 0 && !is.na(note_col_idx)) {
    col_definitions[[length(col_definitions) + 1]] <- list(
      targets = note_col_idx,
      render = DT::JS(
        "function(data, type, row) {",
        "  if (type === 'display' && data !== null && data.length > 60) {",
        "    var cleanText = data.replace(/\"/g, '&quot;').replace(/\\n/g, ' ');",
        "    return '<span title=\"' + cleanText + '\">' + data.substring(0, 60) + '...</span>';",
        "  }",
        "  return data;",
        "}"
      )
    )
  }

  dom_str <- if (show_buttons) 'Blfrtip' else 'lfrtip'

  DT::datatable(
    formatted_df,
    escape = FALSE,
    extensions = if (show_buttons) 'Buttons' else NULL,
    filter = "top",
    options = list(
      dom = dom_str,
      buttons = if (show_buttons) list(
        list(extend = 'copy', title = NULL),
        list(extend = 'csv', filename = clean_filename, title = NULL),
        list(extend = 'excel', filename = clean_filename, title = NULL)
      ) else NULL,
      pageLength = 15,
      lengthMenu = list(c(10, 15, 20, 50, 100, 200, -1), c('10', '15', '20', '50', '100', '200', 'All')),
      columnDefs = col_definitions
    )
  )
}
