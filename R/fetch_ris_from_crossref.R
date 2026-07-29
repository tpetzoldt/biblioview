#' Fetch Bibliography Metadata in RIS Format from Crossref
#'
#' Takes a vector of DOIs and retrieves their bibliographic data formatted as RIS
#' using Crossref Content Negotiation.
#'
#' @param dois Character vector of DOIs.
#' @param email Optional email address for Crossref's polite pool (recommended).
#'
#' @return A character vector of RIS formatted entries.
#' @export
#'
#' @examples
#' \dontrun{
#' ris <- fetch_ris_from_crossref(c("10.32614/R.manuals", "10.32614/CRAN.package.deSolve", "10.1007/s35147-026-2647-x"))
#' write_ris(ris, "export.ris")
#' }
fetch_ris_from_crossref <- function(dois, email = NULL) {
  if (length(dois) == 0) return(character(0))

  # Clean input DOIs cleanly for atomic character vectors
  cleaned_dois <- vapply(dois, clean_doi_string, FUN.VALUE = character(1), USE.NAMES = FALSE)
  valid_idx <- !is.na(cleaned_dois) & cleaned_dois != ""

  if (!any(valid_idx)) {
    warning("No valid DOIs provided.")
    return(character(0))
  }

  target_dois <- cleaned_dois[valid_idx]
  ris_records <- character(length(target_dois))

  # Setup headers
  headers <- c(
    "Accept" = "application/x-research-info-systems",
    "User-Agent" = if (!is.null(email)) {
      sprintf("biblioview/R (mailto:%s)", email)
    } else {
      "biblioview/R (https://github.com/tpetzoldt/biblioview)"
    }
  )

  for (i in seq_along(target_dois)) {
    doi <- target_dois[i]
    url <- paste0("https://doi.org/", utils::URLencode(doi, reserved = TRUE))

    tryCatch({
      req <- httr2::request(url) |>
        httr2::req_headers(!!!headers) |>
        httr2::req_error(is_error = function(resp) FALSE) |>
        httr2::req_perform()

      if (httr2::resp_status(req) == 200) {
        ris_records[i] <- httr2::resp_body_string(req)
      } else {
        warning(sprintf("Failed to fetch DOI %s (HTTP %s)", doi, httr2::resp_status(req)))
        ris_records[i] <- ""
      }
    }, error = function(e) {
      warning(sprintf("Error requesting DOI %s: %s", doi, e$message))
      ris_records[i] <- ""
    })

    # Polite rate limit pause
    if (length(target_dois) > 1) Sys.sleep(0.15)
  }

  # Filter out empty records
  ris_records <- ris_records[ris_records != ""]
  return(ris_records)
}
