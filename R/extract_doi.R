
#' Extract and Normalize DOI from Zotero Item Data
#'
#' Inspects a Zotero item's metadata list for a DOI, checking the primary
#' `DOI` field first, followed by the `extra` field if necessary. Automatically
#' normalizes URL prefixes and validates the '10.' prefix structure.
#'
#' @param data List. The `data` element of a Zotero API item list (e.g., `item$data`).
#'
#' @return Clean character string containing the DOI, or `NULL` if no valid DOI is found.
#'
#' @export
extract_doi <- function(data) {
  # 1. Check standard DOI field first
  if (!is.null(data$DOI) && nzchar(trimws(data$DOI))) {
    cleaned <- clean_doi_string(data$DOI)
    if (!is.null(cleaned)) return(cleaned)
  }

  # 2. Check 'extra' field for 'DOI: 10.xxxx/...' if primary field is blank or invalid
  if (!is.null(data$extra) && nzchar(trimws(data$extra))) {
    doi_match <- regmatches(data$extra, regexpr("10\\.\\d{4,9}/[-._;()/:A-Za-z0-9]+", data$extra))
    if (length(doi_match) > 0) {
      cleaned <- clean_doi_string(doi_match[1])
      if (!is.null(cleaned)) return(cleaned)
    }
  }

  return(NULL)
}
