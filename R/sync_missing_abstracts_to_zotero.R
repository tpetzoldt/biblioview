#' Sync Missing Abstracts Back to Zotero API
#'
#' Scans a Zotero group library (or specific collections) for top-level items
#' missing abstracts, fetches them from external APIs, sanitizes formatting,
#' and writes them back to Zotero.
#'
#' @param group_id Character or Numeric. The Zotero Group ID.
#' @param api_key Character. A Zotero API key with write access to the group.
#' @param collections Character vector (optional). One or more Zotero folder/collection names.
#'   If NULL (default), scans the entire group library.
#' @param force_overwrite Logical. If TRUE, re-fetches and overwrites pre-existing abstracts.
#'   Defaults to FALSE.
#' @param email_contact Character. Contact email used for polite external API pools.
#'   Defaults to environment variable `POLITE_EMAIL`.
#' @param providers Named list of provider functions passed to external APIs.
#'
#' @return A named list containing summary statistics of the sync operation and
#'   details on any unresolvable items.
#'
#' @export
sync_missing_abstracts_to_zotero <- function(group_id,
                                             api_key,
                                             collections = NULL,
                                             force_overwrite = FALSE,
                                             email_contact = Sys.getenv("POLITE_EMAIL"),
                                             providers = list(
                                               epmc            = fetch_abstract_epmc,
                                               crossref        = fetch_abstract_crossref,
                                               openalex        = fetch_abstract_openalex
                                             )) {

  if (missing(group_id) || missing(api_key)) {
    stop("Both `group_id` and a write-enabled `api_key` are required.")
  }

  target_collection_keys <- NULL

  # 1. Resolve folder names AND subcollections recursively
  if (!is.null(collections) && length(collections) > 0) {
    cat("Resolving folder hierarchy from Zotero API...\n")

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

      get_all_children <- function(parent_keys, coll_list) {
        children <- Filter(function(x) {
          !is.null(x$data$parentCollection) && x$data$parentCollection %in% parent_keys
        }, coll_list)

        if (length(children) == 0) return(character(0))
        child_keys <- sapply(children, function(x) x$key)
        return(c(child_keys, get_all_children(child_keys, coll_list)))
      }

      parent_keys <- sapply(matched_colls, function(x) x$key)
      all_child_keys <- get_all_children(parent_keys, all_colls)
      target_collection_keys <- unique(c(parent_keys, all_child_keys))

      cat(sprintf("\u2713 Scoped to folder(s) + descendants (%d collection keys matched)\n",
                  length(target_collection_keys)))
    } else {
      stop("Failed to fetch collections from Zotero API. Check your permissions.")
    }
  }

  # 2. Fetch items using deterministic sorting and filter attachments/notes
  fetch_zotero_endpoint <- function(endpoint_url) {
    items <- list()
    start_index <- 0
    limit <- 100
    total_expected <- Inf

    while (length(items) < total_expected) {
      z_req <- httr2::request(endpoint_url) |>
        httr2::req_headers(`Zotero-API-Key` = api_key) |>
        httr2::req_url_query(
          format    = "json",
          limit     = limit,
          start     = start_index,
          sort      = "dateAdded",    # Keep deterministic pagination
          direction = "asc"
        ) |>
        httr2::req_error(is_error = function(resp) FALSE)

      z_res <- httr2::req_perform(z_req)

      if (httr2::resp_status(z_res) != 200) {
        stop(sprintf("Failed to connect to Zotero API (Status: %d). Check your Group ID and API key permissions.",
                     httr2::resp_status(z_res)))
      }

      headers <- httr2::resp_headers(z_res)
      if (!is.null(headers[["total-results"]])) {
        total_expected <- as.numeric(headers[["total-results"]])
      }

      batch <- httr2::resp_body_json(z_res, simplifyVector = FALSE)
      if (length(batch) == 0) break

      items <- c(items, batch)
      start_index <- start_index + length(batch)
    }

    # Filter out attachment and note item types in R safely
    clean_items <- Filter(function(x) {
      itype <- x$data$itemType
      !is.null(itype) && !itype %in% c("attachment", "note")
    }, items)

    return(clean_items)
  }

  all_items <- list()

  if (!is.null(target_collection_keys)) {
    for (ckey in target_collection_keys) {
      coll_endpoint <- paste0("https://api.zotero.org/groups/", group_id, "/collections/", ckey, "/items/top")
      fetched <- fetch_zotero_endpoint(coll_endpoint)
      all_items <- c(all_items, fetched)
    }
    # Deduplicate items in case of multi-collection assignment
    item_keys <- sapply(all_items, function(x) x$key)
    all_items <- all_items[!duplicated(item_keys)]
  } else {
    base_items_url <- paste0("https://api.zotero.org/groups/", group_id, "/items/top")
    all_items <- fetch_zotero_endpoint(base_items_url)
  }

  total_fetched_raw <- length(all_items)
  cat(sprintf("\u2713 Successfully retrieved %d top-level bibliographic items.\n", total_fetched_raw))

  # 3. Categorize items with and without DOIs
  valid_items  <- Filter(function(x) !is.null(extract_doi(x$data)), all_items)
  no_doi_items <- Filter(function(x) is.null(extract_doi(x$data)), all_items)

  total_records <- length(valid_items)

  if (total_records == 0) {
    cat("\u2713 No items with valid DOIs found for this scope.\n")
    return(invisible(list(
      n_records           = 0,
      n_preexisting       = 0,
      n_retrieved         = 0,
      retrieved_by_source = setNames(as.list(rep(0, length(providers))), names(providers)),
      n_still_empty       = 0,
      unresolved_items    = list()
    )))
  }

  # 4. Identify pre-existing vs missing abstracts based on `force_overwrite`
  has_abstract <- function(x) {
    if (force_overwrite) return(FALSE)
    !is.null(x$data$abstractNote) && nzchar(trimws(x$data$abstractNote))
  }

  preexisting_items <- Filter(has_abstract, valid_items)
  missing_items     <- Filter(function(x) !has_abstract(x), valid_items)

  n_preexisting <- length(preexisting_items)
  n_missing     <- length(missing_items)

  source_counts    <- setNames(rep(0, length(providers)), names(providers))
  n_retrieved      <- 0
  unresolved_list  <- list()

  if (n_missing == 0) {
    cat(sprintf("\n\u2713 All %d records with DOIs already have abstracts!\n", total_records))
  } else {
    cat(sprintf("\nFound %d records with DOIs (%d kept as-is, %d scheduled for enrichment)...\n",
                total_records, n_preexisting, n_missing))

    base_req <- build_base_req(email_contact)

    # 5. Enrich missing/overwritten abstracts
    for (item in missing_items) {
      item_key     <- item$key
      item_version <- item$version

      # Clean and URL-encode for API requests
      clean_doi <- extract_doi(item$data)
      if (is.null(clean_doi)) next

      encoded_doi <- URLencode(clean_doi, reserved = TRUE)

      message(sprintf("\nProcessing Zotero Key: %s | DOI: %s", item_key, clean_doi))

      fetched_abstract  <- NULL
      successful_source <- NULL

      for (provider_name in names(providers)) {
        fetcher_fn   <- providers[[provider_name]]
        raw_abstract <- fetcher_fn(encoded_doi, base_req)

        if (!is.null(raw_abstract) && nzchar(trimws(raw_abstract))) {
          fetched_abstract  <- sanitize_abstract_text(raw_abstract)
          successful_source <- provider_name
          message(sprintf("  -> Abstract found via %s!", provider_name))
          break
        }
      }

      if (!is.null(fetched_abstract)) {
        patch_url <- paste0("https://api.zotero.org/groups/", group_id, "/items/", item_key)
        patch_body <- list(abstractNote = fetched_abstract)

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
        message("  -> Abstract not available in configured open repositories.")

        # Collect unresolvable item metadata
        item_title <- if (!is.null(item$data$title) && nzchar(trimws(item$data$title))) item$data$title else "[No Title]"
        unresolved_list[[length(unresolved_list) + 1]] <- list(
          key   = item_key,
          doi   = clean_doi,
          title = item_title
        )
      }
    }
  }

  n_still_empty <- total_records - (n_preexisting + n_retrieved)

  summary_stats <- list(
    n_records           = total_records,
    n_preexisting       = n_preexisting,
    n_retrieved         = n_retrieved,
    retrieved_by_source = as.list(source_counts),
    n_still_empty       = n_still_empty,
    unresolved_items    = unresolved_list
  )

  # Display Clean Summary Report
  cat("\n==================================================\n")
  cat("          ZOTERO ABSTRACT SYNC SUMMARY            \n")
  cat("==================================================\n")
  if (!is.null(collections)) {
    cat(sprintf("Folders Scoped:                     %s\n", paste(collections, collapse = ", ")))
  }
  cat(sprintf("Force Overwrite Mode:               %s\n", ifelse(force_overwrite, "ENABLED", "DISABLED")))
  cat(sprintf("Total Top-Level Items Fetched:      %d\n", total_fetched_raw))
  cat(sprintf("Items Lacking DOIs (Skipped):       %d\n", length(no_doi_items)))
  cat(sprintf("Total Records Evaluated (with DOIs): %d\n", summary_stats$n_records))
  cat(sprintf("Pre-existing Abstracts Kept:        %d\n", summary_stats$n_preexisting))
  cat(sprintf("Newly Retrieved & Written:          %d\n", summary_stats$n_retrieved))

  for (src in names(summary_stats$retrieved_by_source)) {
    cat(sprintf("   - %-15s:                 %d\n", src, summary_stats$retrieved_by_source[[src]]))
  }

  cat(sprintf("Remaining Empty Abstracts:          %d\n", summary_stats$n_still_empty))
  cat("==================================================\n")

  # Optional reporting for unresolvable items
  if (length(unresolved_list) > 0) {
    cat("\n\u2139 Items lacking available abstracts in open repositories:\n")
    for (unres in unresolved_list) {
      cat(sprintf("   - Key: %s | DOI: %s\n     Title: %s\n",
                  unres$key, unres$doi, substr(unres$title, 1, 60)))
    }
    cat("\n")
  }

  return(invisible(summary_stats))
}
