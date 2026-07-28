
# =========================================================================
# INDIVIDUAL API FETCHERS
# =========================================================================

#' Fetch Abstract from Europe PMC
#' @param doi Clean DOI string.
#' @param req_template httr2 base request.
#' @return Character string containing abstract or NULL if not found.
#' @keywords internal
fetch_abstract_epmc <- function(doi, req_template) {
  epmc_req <- req_template |>
    httr2::req_url("https://www.ebi.ac.uk/europepmc/webservices/rest/search") |>
    httr2::req_url_query(query = paste0("doi:", doi), resultType = "core", format = "json")

  epmc_res <- httr2::req_perform(epmc_req)

  if (httr2::resp_status(epmc_res) == 200) {
    data <- httr2::resp_body_json(epmc_res, simplifyVector = FALSE)
    results <- data$resultList$result
    if (length(results) > 0) {
      abstract <- results[[1]]$abstractText
      if (!is.null(abstract) && nzchar(trimws(abstract))) {
        return(trimws(abstract))
      }
    }
  }
  return(NULL)
}

#' Fetch Abstract from OpenAlex
#' @param doi Clean DOI string.
#' @param req_template httr2 base request.
#' @return Character string containing abstract or NULL if not found.
#' @keywords internal
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
#' @keywords internal
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
