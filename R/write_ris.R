#' Export RIS Entries to File
#'
#' Writes a character vector of RIS records to a file.
#'
#' @param ris_data Character vector of RIS records (e.g. from `fetch_ris_from_crossref`).
#' @param file Path to the destination file.
#'
#' @return The file path invisibly.
#' @export
write_ris <- function(ris_data, file) {
  if (length(ris_data) == 0) {
    warning("No RIS data provided to write.")
    return(invisible(file))
  }

  # Ensure records are properly separated by blank lines if concatenating
  content <- paste(ris_data, collapse = "\n\n")

  writeLines(content, con = file, useBytes = FALSE)
  return(invisible(file))
}
