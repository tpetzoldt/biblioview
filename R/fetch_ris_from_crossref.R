#' Fetch Bibliography Metadata in RIS Format from Crossref
#'
#' Takes a vector of DOIs and retrieves their bibliographic data formatted as RIS
#' using Crossref Content Negotiation. Includes robust error handling and automatic
#' retry logic for network rate-limiting.
#'
#' @param dois Character vector of DOIs.
#' @param email Optional email address for Crossref's polite pool (recommended).
#'
#' @return A character vector of RIS formatted entries.
#' @export
#' @seealso [write_ris()]
#' @examples
#' \dontrun{
#' ris <- fetch_ris_from_crossref(
#'   dois = c("10.1016/j.ecolmodel.2020.109282", "10.1111/gcb.15000"),
#'   email = "user@example.com"
#' )
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

  # Resolve email for polite pool (fallback to Sys.getenv if available)
  if (is.null(email) || email == "") {
    email <- Sys.getenv("POLITE_EMAIL", unset = "")
  }

  # Build User-Agent header according to Crossref polite pool rules
  ua_string <- if (nzchar(email)) {
    sprintf("biblioview/R (https://github.com/tpetzoldt/biblioview; mailto:%s)", email)
  } else {
    "biblioview/R (https://github.com/tpetzoldt/biblioview)"
  }

  headers <- c(
    "Accept"     = "application/x-research-info-systems",
    "User-Agent" = ua_string
  )

  for (i in seq_along(target_dois)) {
    doi <- target_dois[i]
    url <- paste0("https://doi.org/", utils::URLencode(doi, reserved = TRUE))

    tryCatch({
      resp <- httr2::request(url) |>
        httr2::req_headers(!!!headers) |>
        # Configure automatic retry on transient rate limits (429) or server errors (5xx)
        httr2::req_retry(
          max_tries = 3,
          backoff = function(attempt) 0.5 * (2 ^ (attempt - 1)),
          is_transient = function(resp) {
            httr2::resp_status(resp) %in% c(429, 500, 502, 503, 504)
          }
        ) |>
        # Don't crash R execution on HTTP errors; let us evaluate status manually
        httr2::req_error(is_error = function(resp) FALSE) |>
        httr2::req_perform()

      status <- httr2::resp_status(resp)

      if (status == 200) {
        ris_records[i] <- httr2::resp_body_string(resp)
      } else if (status == 404) {
        warning(sprintf("DOI not found on Crossref: %s (HTTP 404)", doi))
        ris_records[i] <- ""
      } else {
        warning(sprintf("Failed to fetch DOI %s (HTTP %s)", doi, status))
        ris_records[i] <- ""
      }

    }, error = function(e) {
      warning(sprintf("Network or parsing error for DOI %s: %s", doi, e$message))
      ris_records[i] <- ""
    })

    # Baseline polite pause between consecutive requests
    if (length(target_dois) > 1 && i < length(target_dois)) {
      Sys.sleep(0.15)
    }
  }

  # Filter out empty records
  ris_records <- ris_records[ris_records != ""]
  return(ris_records)
}
