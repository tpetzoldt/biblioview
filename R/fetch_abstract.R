# =========================================================================
# INDIVIDUAL API FETCHERS for Abstract Enrichment
# =========================================================================


#' Fetch Abstract from Europe PMC API
#'
#' Queries Europe PMC for an article's abstract using its DOI. Requires
#' `resultType = "core"` in the query parameters to ensure abstract text is included.
#'
#' @param encoded_doi Character string. Clean or URL-encoded DOI.
#' @param base_req An httr2 request object configured with user-agent/headers.
#'
#' @return Character string containing abstract text, or `NULL` if not found.
#' @export
fetch_abstract_epmc <- function(encoded_doi, base_req) {
  clean_doi <- URLdecode(encoded_doi)

  res <- tryCatch({
    base_req |>
      httr2::req_url("https://www.ebi.ac.uk/europepmc/webservices/rest/search") |>
      httr2::req_url_query(
        query      = sprintf('DOI:"%s"', clean_doi),
        format     = "json",
        resultType = "core"
      ) |>
      httr2::req_perform() |>
      httr2::resp_body_json()
  }, error = function(e) NULL)

  if (is.null(res)) return(NULL)

  result_list <- res$resultList$result
  if (length(result_list) > 0 && !is.null(result_list[[1]]$abstractText)) {
    abs_text <- result_list[[1]]$abstractText
    if (nzchar(trimws(abs_text))) return(abs_text)
  }

  return(NULL)
}


#' Fetch Abstract from OpenAlex
#' @param doi Clean DOI string.
#' @param req_template httr2 base request.
#' @return Character string containing abstract or NULL if not found.
#' @export
fetch_abstract_openalex <- function(doi, req_template) {
  oa_req <- req_template |>
    httr2::req_url(paste0("https://api.openalex.org/works/doi:", doi))

  oa_res <- httr2::req_perform(oa_req)

  if (httr2::resp_status(oa_res) == 200) {
    data <- httr2::resp_body_json(oa_res, simplifyVector = FALSE)
    inv_index <- data$abstract_inverted_index

    if (!is.null(inv_index) && length(inv_index) > 0 && length(unlist(inv_index)) > 0) {
      positions <- unlist(inv_index, use.names = FALSE) + 1
      words     <- rep(names(inv_index), lengths(inv_index))

      word_list <- character(max(positions))
      word_list[positions] <- words

      return(trimws(paste(word_list, collapse = " ")))
    }
  }
  return(NULL)
}

#' Fetch Abstract from Crossref
#' @param doi Clean DOI string.
#' @param req_template httr2 base request.
#' @return Character string containing abstract or NULL if not found.
#' @export
fetch_abstract_crossref <- function(doi, req_template) {
  cr_req <- req_template |>
    httr2::req_url(paste0("https://api.crossref.org/works/", doi))

  cr_res <- httr2::req_perform(cr_req)

  if (httr2::resp_status(cr_res) == 200) {
    data <- httr2::resp_body_json(cr_res, simplifyVector = FALSE)
    abstract <- data$message$abstract
    if (!is.null(abstract) && nzchar(trimws(abstract))) {
      return(trimws(gsub("<[^>]+>", "", abstract)))
    }
  }
  return(NULL)
}


#' Fetch Abstract from Semantic Scholar
#' @param doi Clean DOI string.
#' @param base_req httr2 base request.
#' @return Character string containing abstract or NULL if not found.
#' @export
fetch_abstract_semanticscholar <- function(doi, base_req) {
  url <- paste0("https://api.semanticscholar.org/graph/v1/paper/DOI:", doi, "?fields=abstract")
  res <- tryCatch({
    base_req |>
      httr2::req_url(url) |>
      httr2::req_perform() |>
      httr2::resp_body_json()
  }, error = function(e) NULL)

  if (!is.null(res$abstract) && nzchar(trimws(res$abstract))) {
    return(res$abstract)
  }
  return(NULL)
}
