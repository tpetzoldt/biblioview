#' Clean and Normalize DOI Strings
#'
#' Strips common URL prefixes (including bare 'doi.org/'), leading/trailing
#' whitespace, and verifies that the resulting string follows standard DOI format.
#'
#' @param raw_doi Character string containing a raw DOI or DOI URL.
#'
#' @return Character string containing the clean DOI (e.g., "10.1579/..."),
#'   or `NULL` if input is invalid or missing the '10.' prefix.
#'
#' @export
clean_doi_string <- function(raw_doi) {
  if (is.null(raw_doi) || !nzchar(trimws(raw_doi))) return(NULL)

  # Trim spaces and remove prefix variations: http(s), dx., bare doi.org/, or doi:
  clean <- trimws(raw_doi)
  clean <- sub("^(https?://)?(dx\\.)?doi\\.org/", "", clean, ignore.case = TRUE)
  clean <- sub("^doi:\\s*", "", clean, ignore.case = TRUE)

  # Verify valid DOI prefix (must start with '10.')
  if (!grepl("^10\\.", clean)) return(NULL)

  return(clean)
}
