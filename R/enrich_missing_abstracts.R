enrich_missing_abstracts <- function(df) {

  email_contact <- "your.email@domain.com" # Replace with your email

  cat("Starting abstract enrichment process...\n")

  for (i in 1:nrow(df)) {
    if (is.na(df$Abstract[i]) || trimws(df$Abstract[i]) == "") {

      raw_doi <- sub("^https://doi.org/", "", df$DOI[i])
      if (!grepl("^10\\.", raw_doi)) next

      message(paste("Row", i, "- Attempting to fetch abstract for DOI:", raw_doi))

      abstract_found <- FALSE

      # =========================================================================
      # LAYER 1: TRY CROSSREF
      # =========================================================================
      crossref_url <- paste0("https://api.crossref.org/works/", URLencode(raw_doi, reserved = TRUE))
      cr_res <- GET(crossref_url, user_agent(paste0("mailto:", email_contact)))

      if (status_code(cr_res) == 200) {
        cr_data <- fromJSON(content(cr_res, "text", encoding = "UTF-8"), simplifyVector = FALSE)
        cr_abstract <- cr_data$message$abstract
        if (!is.null(cr_abstract) && nzchar(trimws(cr_abstract))) {
          df$Abstract[i] <- trimws(gsub("<[^>]+>", "", cr_abstract))
          abstract_found <- TRUE
          message("  -> Success! Abstract retrieved from Crossref.")
        }
      }

      # =========================================================================
      # LAYER 2: TRY OPENALEX
      # =========================================================================
      if (!abstract_found) {
        openalex_url <- paste0("https://api.openalex.org/works/https://doi.org/", raw_doi)
        oa_res <- GET(openalex_url, user_agent(paste0("mailto:", email_contact)))

        if (status_code(oa_res) == 200) {
          oa_data <- fromJSON(content(oa_res, "text", encoding = "UTF-8"), simplifyVector = FALSE)
          inv_index <- oa_data$abstract_inverted_index
          if (!is.null(inv_index) && length(inv_index) > 0) {
            word_list <- vector("character", length = max(unlist(inv_index)) + 1)
            for (word in names(inv_index)) {
              word_list[unlist(inv_index[[word]]) + 1] <- word
            }
            df$Abstract[i] <- trimws(paste(word_list, collapse = " "))
            abstract_found <- TRUE
            message("  -> Success! Abstract retrieved from OpenAlex.")
          }
        }
      }

      # =========================================================================
      # LAYER 3: TRY EUROPE PMC (Excellent for Environmental & Agri Science)
      # =========================================================================
      if (!abstract_found) {
        # Europe PMC query matching the exact DOI format
        epmc_url <- paste0("https://www.ebi.ac.uk/europepmc/webservices/rest/search",
                           "?query=doi:", URLencode(raw_doi, reserved = TRUE),
                           "&format=json")

        epmc_res <- GET(epmc_url, user_agent(paste0("mailto:", email_contact)))

        if (status_code(epmc_res) == 200) {
          epmc_data <- fromJSON(content(epmc_res, "text", encoding = "UTF-8"), simplifyVector = FALSE)
          results   <- epmc_data$resultList$result

          if (length(results) > 0) {
            epmc_abstract <- results[[1]]$abstractText
            if (!is.null(epmc_abstract) && nzchar(trimws(epmc_abstract))) {
              df$Abstract[i] <- trimws(epmc_abstract)
              abstract_found <- TRUE
              message("  -> Success! Abstract retrieved from Europe PMC.")
            }
          }
        }
      }

      # Output status if completely missing across all repositories
      if (!abstract_found) {
        message("  -> Could not locate abstract in any metadata repository.")
      }

      Sys.sleep(0.5) # Polite API cooldown pacing
    }
  }

  return(df)
}
