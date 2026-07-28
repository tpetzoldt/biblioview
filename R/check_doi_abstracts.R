#' Check Abstract Availability Across Providers for a Single DOI
#'
#' Debugging utility to query external APIs directly for a specific DOI
#' and print abstract availability and character length.
#'
#' @param doi Character string. Raw or clean DOI (e.g., "10.1007/s10452-015-9564-x").
#' @param email_contact Character. Contact email for API headers.
#'
#' @return A named list of retrieved abstracts (or NULL per provider).
#'
#' @examples
#' # 1. PLOS ONE: "phyloseq: An R Package for Reproducible Interactive Analysis..."
#' check_doi_abstracts("10.1371/journal.pone.0061217")
#'
#' # 2. Journal of Statistical Software: "Data Validation Infrastructure for R" (validate package)
#' check_doi_abstracts("10.18637/jss.v097.i10")
#'
#' # 3. PLOS ONE: "Dose-Response Analysis Using R" (drc package)
#' check_doi_abstracts("10.1371/journal.pone.0146021")
#'
#' @export
check_doi_abstracts <- function(doi, email_contact = Sys.getenv("POLITE_EMAIL")) {
  clean <- clean_doi_string(doi)
  if (is.null(clean)) {
    stop("Invalid DOI string provided.")
  }

  encoded <- URLencode(clean, reserved = TRUE)

  # Pass "" to initialize httr2 request template
  base_req <- httr2::request("") |>
    httr2::req_user_agent(sprintf("R-Zotero-Sync-Debug (%s)", email_contact))

  providers <- list(
    epmc            = fetch_abstract_epmc,
    crossref        = fetch_abstract_crossref,
    openalex        = fetch_abstract_openalex,
    semanticscholar = fetch_abstract_semanticscholar
  )

  cat(sprintf("\n=== DIRECT API CHECK FOR DOI: %s ===\n", clean))

  results <- list()
  for (pname in names(providers)) {
    fn <- providers[[pname]]
    abs <- tryCatch(fn(encoded, base_req), error = function(e) NULL)
    results[[pname]] <- abs

    if (!is.null(abs) && nzchar(trimws(abs))) {
      cat(sprintf("  %-16s : \u2713 FOUND (%d characters)\n", pname, nchar(abs)))
    } else {
      cat(sprintf("  %-16s : \u2717 Not available / NULL\n", pname))
    }
  }
  cat("====================================================\n\n")

  return(invisible(results))
}
