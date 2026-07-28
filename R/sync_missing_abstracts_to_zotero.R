#' Clean and Sanitize Abstract Text
#'
#' Strips or converts HTML/XML markup (like sub/sup tags and JATS math)
#' into clean plain text for Zotero notes.
#'
#' @param text Character string containing raw abstract text.
#' @return Cleaned character string.
#' @keywords internal
sanitize_abstract_text <- function(text) {
  if (is.null(text) || !nzchar(text)) return(NULL)

  clean <- text

  # 1. Standardize line breaks and non-breaking spaces
  clean <- gsub("\r\n|\r", "\n", clean)
  clean <- gsub("&nbsp;", " ", clean, fixed = TRUE)

  # 2. Handle XML/JATS math & formula tags before stripping HTML
  clean <- gsub("<inline-formula>.*?</inline-formula>", "[formula]", clean, ignore.case = TRUE)
  clean <- gsub("</?sub-script>", "", clean, ignore.case = TRUE)
  clean <- gsub("</?super-script>", "", clean, ignore.case = TRUE)

  # 3. Convert sub/sup tags to plain text (e.g., H<sub>2</sub>O -> H2O)
  clean <- gsub("<sub>(.*?)</sub>", "\\1", clean, ignore.case = TRUE)
  clean <- gsub("<sup>(.*?)</sup>", "\\1", clean, ignore.case = TRUE)

  # 4. Strip any remaining HTML/XML tags safely
  if (requireNamespace("rvest", quietly = TRUE)) {
    clean <- tryCatch({
      clean |>
        xml2::read_html() |>
        rvest::html_text2()
    }, error = function(e) {
      gsub("<[^>]+>", "", clean)
    })
  } else {
    clean <- gsub("<[^>]+>", "", clean)
  }

  # 5. Clean up extra whitespace created by tag removal
  clean <- gsub("[ \t]+", " ", clean)
  clean <- trimws(clean)

  return(clean)
}

#' Sync Missing Abstracts Back to Zotero API
#'
#' Scans a Zotero group library (or specific collections) for top-level items
#' missing abstracts, fetches them from external APIs, sanitizes formatting,
#' and writes them back to Zotero.
#'
#' @param group_id Character or Numeric. The Zotero Group ID.
#' @param api_key Character. A Zotero API key with **write access** to the group.
#' @param collections Character vector (optional). One or more Zotero folder/collection names.
#'   If NULL (default), scans the entire group library.
#' @param force_overwrite Logical. If TRUE, re-fetches and overwrites pre-existing abstracts.
#'   Defaults to FALSE.
#' @param email_contact Character. Contact email used for polite external API pools.
#'   Defaults to environment variable `POLITE_EMAIL`.
#' @param providers Named list of provider functions passed to external APIs.
#'
#' @return A named list containing summary statistics of the sync operation.
#'
#' @export
sync_missing_abstracts_to_zotero <- function(group_id,
                                             api_key,
                                             collections = NULL,
                                             force_overwrite = FALSE,
                                             email_contact = Sys.getenv("POLITE_EMAIL"),
                                             providers = list(
                                               epmc     = fetch_abstract_epmc,
                                               crossref = fetch_abstract_crossref,
                                               openalex = fetch_abstract_openalex
                                             )) {

  if (missing(group_id) || missing(api_key)) {
    stop("Both `group_id` and a write-enabled `api_key` are required.")
  }

  target_collection_keys <- NULL

  # 1. Resolve folder names to Collection Keys if specified
  if (!is.null(collections) && length(collections) > 0) {
    cat("Resolving folder names to Zotero collection keys...\n")

    coll_url <- paste0("https://api.zotero.org/groups/", group_id, "/collections")
    coll_req <- httr2::request(coll_url) |>
      httr2::req_headers(`Zotero-API-Key` = api_key) |>
      httr2::req_url_query(format = "json", limit = 100) |>
      httr2::req_error(is_error = function(resp) FALSE)

    coll_res <- httr2::req_perform(coll_req)

    if (httr2::resp_status(coll_res) == 200) {
      all_colls <- httr2::resp_body_json(coll_res, simplifyVector = FALSE)

      matched_colls <- Filter(function(x) x$data$name %in% collections, all_colls)

      if (length(matched_colls) == 0) {
        stop(sprintf("None of the specified collections (%s) were found in this library.",
                     paste(collections, collapse = ", ")))
      }

      found_names <- sapply(matched_colls, function(x) x$data$name)
      missing_names <- setdiff(collections, found_names)

      if (length(missing_names) > 0) {
        warning(sprintf("The following folders were not found in Zotero and will be ignored: %s",
                        paste(missing_names, collapse = ", ")))
      }

      target_collection_keys <- sapply(matched_colls, function(x) x$key)
      cat(sprintf("✓ Scoping sync to folder(s): %s\n", paste(found_names, collapse = ", ")))
    } else {
      stop("Failed to fetch collections from Zotero API. Check your permissions.")
    }
  }

  cat("Fetching library metadata from Zotero Group:", group_id, "...\n")

  # 2. Fetch ALL top-level items with pagination loop
  all_items <- list()
  start_index <- 0
  limit <- 100
  more_results <- TRUE

  base_items_url <- paste0("https://api.zotero.org/groups/", group_id, "/items/top")

  while (more_results) {
    z_req <- httr2::request(base_items_url) |>
      httr2::req_headers(`Zotero-API-Key` = api_key) |>
      httr2::req_url_query(format = "json", limit = limit, start = start_index) |>
      httr2::req_error(is_error = function(resp) FALSE)

    z_res <- httr2::req_perform(z_req)

    if (httr2::resp_status(z_res) != 200) {
      stop("Failed to connect to Zotero API. Check your Group ID and API key permissions.")
    }

    batch <- httr2::resp_body_json(z_res, simplifyVector = FALSE)
    all_items <- c(all_items, batch)

    if (length(batch) < limit) {
      more_results <- FALSE
    } else {
      start_index <- start_index + limit
    }
  }

  cat(sprintf("✓ Fetched %d total items across all pages.\n", length(all_items)))

  # 3. Filter items by collection scope and valid DOIs
  valid_items <- Filter(function(x) {
    data <- x$data
    has_doi <- !is.null(data$DOI) && nzchar(trimws(data$DOI))

    in_target_scope <- TRUE
    if (!is.null(target_collection_keys)) {
      item_colls <- unlist(data$collections)
      in_target_scope <- any(target_collection_keys %in% item_colls)
    }

    return(has_doi && in_target_scope)
  }, all_items)

  total_records <- length(valid_items)

  if (total_records == 0) {
    cat("✓ No matching items with valid DOIs found for this scope.\n")
    return(invisible(list(
      n_records = 0,
      n_preexisting = 0,
      n_retrieved = 0,
      retrieved_by_source = setNames(as.list(rep(0, length(providers))), names(providers)),
      n_still_empty = 0
    )))
  }

  # 4. Identify pre-existing vs missing abstracts based on `force_overwrite`
  has_abstract <- function(x) {
    if (force_overwrite) return(FALSE) # Force all records into the missing/enrichment queue
    !is.null(x$data$abstractNote) && nzchar(trimws(x$data$abstractNote))
  }

  preexisting_items <- Filter(has_abstract, valid_items)
  missing_items     <- Filter(function(x) !has_abstract(x), valid_items)

  n_preexisting <- length(preexisting_items)
  n_missing     <- length(missing_items)

  source_counts <- setNames(rep(0, length(providers)), names(providers))
  n_retrieved   <- 0

  if (n_missing == 0) {
    cat(sprintf("✓ All %d records in scope already have abstracts!\n", total_records))
  } else {
    cat(sprintf("Found %d records with DOIs. %d kept as-is, %d scheduled for enrichment...\n",
                total_records, n_preexisting, n_missing))

    base_req <- build_base_req(email_contact)

    # 5. Enrich missing/overwritten abstracts
    for (item in missing_items) {
      item_key     <- item$key
      item_version <- item$version
      raw_doi      <- trimws(item$data$DOI)
      clean_doi    <- sub("^https?://(dx\\.)?doi\\.org/", "", raw_doi)

      if (!grepl("^10\\.", clean_doi)) next

      message(sprintf("\nProcessing Zotero Key: %s | DOI: %s", item_key, clean_doi))

      fetched_abstract <- NULL
      successful_source <- NULL

      # Query external providers
      for (provider_name in names(providers)) {
        fetcher_fn <- providers[[provider_name]]
        raw_abstract <- fetcher_fn(clean_doi, base_req)

        if (!is.null(raw_abstract) && nzchar(trimws(raw_abstract))) {
          # Clean HTML/XML tags and chemical formulas
          fetched_abstract <- sanitize_abstract_text(raw_abstract)
          successful_source <- provider_name
          message(sprintf("  -> Abstract found via %s!", provider_name))
          break
        }
      }

      # Write back to Zotero via PATCH if found
      if (!is.null(fetched_abstract)) {
        patch_url <- paste0("https://api.zotero.org/groups/", group_id, "/items/", item_key)

        patch_body <- list(
          abstractNote = fetched_abstract
        )

        patch_req <- httr2::request(patch_url) |>
          httr2::req_headers(
            `Zotero-API-Key` = api_key,
            `If-Unmodified-Since-Version` = as.character(item_version)
          ) |>
          httr2::req_body_json(patch_body) |>
          httr2::req_method("PATCH") |>
          httr2::req_error(is_error = function(resp) FALSE)

        patch_res <- httr2::req_perform(patch_req)
        status    <- httr2::resp_status(patch_res)

        if (status == 204 || status == 200) {
          message("  -> Successfully updated in Zotero library!")
          n_retrieved <- n_retrieved + 1
          source_counts[successful_source] <- source_counts[successful_source] + 1
        } else if (status == 412) {
          warning(sprintf("  -> Failed to update key %s: Item was modified elsewhere.", item_key))
        } else {
          warning(sprintf("  -> Zotero write failed for key %s (Status: %d).", item_key, status))
        }

        Sys.sleep(0.2)
      } else {
        message("  -> Abstract not available in configured repositories.")
      }
    }
  }

  n_still_empty <- total_records - (n_preexisting + n_retrieved)

  # 6. Construct summary stats
  summary_stats <- list(
    n_records           = total_records,
    n_preexisting       = n_preexisting,
    n_retrieved         = n_retrieved,
    retrieved_by_source = as.list(source_counts),
    n_still_empty       = n_still_empty
  )

  # Print summary report
  cat("\n==================================================\n")
  cat("          ZOTERO ABSTRACT SYNC SUMMARY            \n")
  cat("==================================================\n")
  if (!is.null(collections)) {
    cat(sprintf("Folders Scoped:                     %s\n", paste(collections, collapse = ", ")))
  }
  cat(sprintf("Force Overwrite Mode:               %s\n", ifelse(force_overwrite, "ENABLED", "DISABLED")))
  cat(sprintf("Total Records Evaluated (with DOIs): %d\n", summary_stats$n_records))
  cat(sprintf("Pre-existing Abstracts Kept:        %d\n", summary_stats$n_preexisting))
  cat(sprintf("Newly Retrieved & Written:          %d\n", summary_stats$n_retrieved))

  for (src in names(summary_stats$retrieved_by_source)) {
    cat(sprintf("   - %-15s:                 %d\n", src, summary_stats$retrieved_by_source[[src]]))
  }

  cat(sprintf("Remaining Empty Abstracts:          %d\n", summary_stats$n_still_empty))
  cat("==================================================\n\n")

  return(invisible(summary_stats))
}
