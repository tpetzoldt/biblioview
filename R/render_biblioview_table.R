#' Create Standard Biblioview DataTable
#'
#' Handles hyperlink formatting, JS tooltips, column truncation,
#' and export button configurations uniformly.
#'
#' @param df Data frame containing reference entries.
#' @param title Character. Export filename prefix.
#' @param show_buttons Logical. Include Copy/CSV/Excel export buttons.
#'
#' @importFrom DT datatable JS
#' @export
render_biblioview_table <- function(df, title = "export", show_buttons = TRUE) {
  clean_filename <- gsub("[^a-zA-Z0-9_-]", "_", title)
  formatted_df   <- format_hyperlinks(df)

  abstract_col_idx <- which(tolower(names(formatted_df)) == "abstract")
  note_col_idx     <- which(tolower(names(formatted_df)) %in% c("note", "extra_note"))

  col_definitions <- list()

  # JS Truncation for Abstracts (Note: DT column indices are 0-based)
  if (length(abstract_col_idx) > 0 && !is.na(abstract_col_idx[1])) {
    col_definitions[[length(col_definitions) + 1]] <- list(
      targets = abstract_col_idx[1],
      render = DT::JS(
        "function(data, type, row) {",
        "  if (type === 'display' && data !== null && data.length > 200) {",
        "    var cleanTooltip = data.replace(/\"/g, '&quot;').replace(/\\n/g, ' ');",
        "    return '<span class=\"expandable-cell\" title=\"' + cleanTooltip + '\" data-fulltext=\"' + encodeURIComponent(data) + '\" data-type=\"Note\" style=\"border-bottom: 1px dashed #6c757d; cursor: pointer;\">' + data.substring(0, 200) + '... &#x1F50D;</span>';",
        "  }",
        "  return data;",
        "}"
      )
    )
  }

  # JS Truncation for Notes
  if (length(note_col_idx) > 0 && !is.na(note_col_idx[1])) {
    col_definitions[[length(col_definitions) + 1]] <- list(
      targets = note_col_idx[1],
      render = DT::JS(
        "function(data, type, row) {",
        "  if (type === 'display' && data !== null && data.length > 100) {",
        "    var cleanTooltip = data.replace(/\"/g, '&quot;').replace(/\\n/g, ' ');",
        "    return '<span class=\"expandable-cell\" title=\"' + cleanTooltip + '\" data-fulltext=\"' + encodeURIComponent(data) + '\" data-type=\"Note\" style=\"border-bottom: 1px dashed #6c757d; cursor: pointer;\">' + data.substring(0, 150) + '... &#x1F50D;</span>';",
        "  }",
        "  return data;",
        "}"
      )
    )
  }

  dom_str <- if (show_buttons) 'Blfrtip' else 'lfrtip'

  # Set up datatable arguments dynamically to avoid 'extensions' error
  dt_args <- list(
    data = formatted_df,
    colnames = c(URL = "DOI", Citation = "APA_Citation"),
    escape = FALSE,
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

  # Only include extensions vector when show_buttons is TRUE
  if (show_buttons) {
    dt_args$extensions <- "Buttons"
  }

  do.call(DT::datatable, dt_args)
}
