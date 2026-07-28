#' Extract DOI from Zotero Item Data
#'
#' Inspects a Zotero item's metadata list for a DOI, checking the primary
#' `DOI` field first, followed by the `extra` field if necessary.
#'
#' @param data List. The `data` element of a Zotero API item list (e.g., `item$data`).
#'
#' @return Character string containing the clean DOI, or `NULL` if no valid DOI is found.
#'
#' @keywords internal
#' @noRd
extract_doi <- function(data) {
  # Check standard DOI field first
  if (!is.null(data$DOI) && nzchar(trimws(data$DOI))) {
    return(trimws(data$DOI))
  }

  # Check 'extra' field for 'DOI: 10.xxxx/...'
  if (!is.null(data$extra) && nzchar(trimws(data$extra))) {
    doi_match <- regmatches(data$extra, regexpr("10\\.\\d{4,9}/[-._;()/:A-Za-z0-9]+", data$extra))
    if (length(doi_match) > 0) {
      return(doi_match[1])
    }
  }

  return(NULL)
}
