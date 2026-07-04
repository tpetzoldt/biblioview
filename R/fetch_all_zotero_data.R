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
#'
#' @examples
#' \dontrun{
#' # Fetch everything
#' fetch_all_zotero_data(group_id = "1234567", api_key = "secret_key")
#' 
#' # Fetch only a specific folder subset
#' fetch_all_zotero_data(group_id = "1234567", api_key = "secret_key", collection_id = "A8F3X2Z9")
#' }
fetch_all_zotero_data <- function(group_id, api_key, collection_id = NULL) {

  # 1. Fetch collection definitions to map keys back to human-readable names
  coll_url <- paste0("https://api.zotero.org/groups/", group_id, "/collections")
  coll_res <- httr::GET(coll_url, httr::add_headers("Zotero-API-Key" = api_key))

  if (httr::status_code(coll_res) != 200) stop("Could not retrieve group collections.")

  collections_raw <- jsonlite::fromJSON(
    httr::content(coll_res, "text", encoding = "UTF-8"), 
    simplifyVector = FALSE
  )

  # Filter the discovered list if the user requested a specific subset
  if (!is.null(collection_id)) {
    collections_raw <- Filter(function(coll) coll$key %in% collection_id, collections_raw)
  }

  if (length(collections_raw) == 0) {
    warning("No matching collections found for the provided criteria.")
    return(data.frame())
  }

  master_list <- list()

  # 2. Loop over every filtered sub-collection
  for (coll in collections_raw) {
    coll_name <- coll$data$name
    coll_key  <- coll$key

    message(paste("Processing collection:", coll_name))

    # Fetch all items across all pages for this sub-collection
    coll_data <- fetch_collection_items(group_id, api_key, coll_key)

    if (nrow(coll_data) > 0) {
      # Append the tracking column requested
      coll_data$Sub_Collection <- coll_name
      master_list[[length(master_list) + 1]] <- coll_data
    }
  }

  # 3. Combine into the exact requested table structure using native R pipes
  if (length(master_list) > 0) {
    final_df <- bind_rows(master_list) |>
      select(Sub_Collection, Authors, Year, Title, DOI, APA_Citation, Abstract)

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
