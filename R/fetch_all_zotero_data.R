#' Fetch All Data from a Zotero Group Library
#'
#' Loops through specified or all collections in a Zotero group library, retrieves
#' their items, formats them into a tabular structure, and appends the collection name.
#'
#' @param group_id Character or numeric. The target Zotero Group ID.
#' @param api_key Character. Your secret Zotero Web API v3 key.
#' @param collection_id Optional character vector. Unique Zotero collection keys (hashes)
#'   to pull down. If \code{NULL} (default), all collections in the library are processed.
#'
#' @return A data frame containing structured reference data with columns:
#'   Sub_Collection, Authors, Year, Title, DOI, APA_Citation, and Abstract.
#' @export
fetch_all_zotero_data <- function(group_id, api_key, collection_id = NULL) {

  # 1. Fetch ALL collection definitions to build a global relationship map
  coll_url <- paste0("https://api.zotero.org/groups/", group_id, "/collections")
  coll_res <- httr::GET(coll_url, httr::add_headers("Zotero-API-Key" = api_key))

  if (httr::status_code(coll_res) != 200) stop("Could not retrieve group collections.")

  # This contains EVERY folder in the library
  all_collections_in_library <- jsonlite::fromJSON(
    httr::content(coll_res, "text", encoding = "UTF-8"),
    simplifyVector = FALSE
  )

  # Determine our execution targets based on what the user selected in the UI
  if (!is.null(collection_id)) {
    target_collections <- Filter(function(coll) coll$key %in% collection_id, all_collections_in_library)
  } else {
    target_collections <- all_collections_in_library
  }

  if (length(target_collections) == 0) {
    warning("No matching collections found for the provided criteria.")
    return(data.frame())
  }

  master_list <- list()

  # Keep track of keys we have already physically downloaded across the entire execution
  # to guarantee a folder is NEVER fetched twice under any circumstance!
  already_fetched_keys <- c()

  # 2. LINEAR EXECUTION LOOP WITH DYNAMIC PATH BUILDING
  for (coll in target_collections) {
    coll_name <- coll$data$name
    coll_key  <- coll$key

    # Start a collection bucket for this specific folder track
    target_keys <- coll_key

    # Find all immediate children using the GLOBAL library map
    child_collections <- Filter(function(x) {
      is.character(x$data$parentCollection) && x$data$parentCollection == coll_key
    }, all_collections_in_library)

    if (length(child_collections) > 0) {
      child_keys <- sapply(child_collections, function(x) x$key)
      target_keys <- c(target_keys, child_keys)
    }

    target_keys <- unique(target_keys)
    target_keys <- target_keys[!(target_keys %in% already_fetched_keys)]

    if (length(target_keys) == 0) next

    # Run the flat linear download loop
    for (key in target_keys) {

      # --- NEW PATH-BUILDING LOGIC ---
      # Find the specific folder configuration for the current key
      current_folder <- Filter(function(x) x$key == key, all_collections_in_library)[[1]]
      current_name   <- current_folder$data$name

      # Check if this item physically lives inside a child folder
      if (!is.null(current_folder$data$parentCollection) && current_folder$data$parentCollection != FALSE && current_folder$data$parentCollection != "") {
        # Fetch the parent's definition block to grab its text name
        parent_key   <- current_folder$data$parentCollection
        parent_match <- Filter(function(x) x$key == parent_key, all_collections_in_library)

        if (length(parent_match) > 0) {
          # Option A: Full Multi-Level Path Display (e.g., "Literature > Methods")
          display_path <- paste0(parent_match[[1]]$data$name, " > ", current_name)
        } else {
          display_path <- current_name
        }
      } else {
        # This is already a root-level top folder
        display_path <- current_name
      }
      # --------------------------------

      message(paste("Processing folder:", display_path))
      coll_data <- fetch_collection_items(group_id, api_key, key)

      if (nrow(coll_data) > 0) {
        # Assign the detailed breadcrumb string to your data frame column
        coll_data$Sub_Collection <- display_path
        master_list[[length(master_list) + 1]] <- coll_data
      }

      already_fetched_keys <- c(already_fetched_keys, key)
    }
  }

  # 3. Combine into the exact requested table structure using native R pipes
  if (length(master_list) > 0) {
    final_df <- bind_rows(master_list) |>
      select(Sub_Collection, Authors, Year, Title, DOI, APA_Citation, Abstract, extra)

    # Separate items with a valid DOI from those without
    has_doi  <- final_df |> dplyr::filter(!is.na(DOI) & DOI != "" & DOI != "NA")
    no_doi   <- final_df |> dplyr::filter(is.na(DOI) | DOI == "" | DOI == "NA")

    # Only deduplicate the rows that have a DOI
    deduped_doi <- has_doi |> distinct(DOI, .keep_all = TRUE)

    # Combine them back together
    return(bind_rows(deduped_doi, no_doi))

  } else {
    return(data.frame())
  }
}
