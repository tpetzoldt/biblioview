#' Fetch All Data from a Zotero Group Library
#'
#' Loops through all collections in a specified Zotero group library, retrieves
#' their items, formats them into a tabular structure, and appends the collection name.
#'
#' @param group_id Character or numeric. The target Zotero Group ID.
#' @param api_key Character. Your secret Zotero Web API v3 key.
#'
#' @return A data frame containing structured reference data with columns:
#'   Sub_Collection, Authors, Year, Title, DOI, APA_Citation, and Abstract.
#' @export
#'
#' @examples
#' \dontrun{
#' fetch_all_zotero_data(group_id = "1234567", api_key = "secret_key")
#' }

fetch_all_zotero_data <- function(group_id, api_key) {

  # 1. Fetch all collections to discover keys and names
  coll_url <- paste0("https://api.zotero.org/groups/", group_id, "/collections")
  coll_res <- httr::GET(coll_url, httr::add_headers("Zotero-API-Key" = api_key))

  if (httr::status_code(coll_res) != 200) stop("Could not retrieve group collections.")

  collections_raw <- jsonlite::fromJSON(httr::content(coll_res, "text", encoding = "UTF-8"), simplifyVector = FALSE)

  master_list <- list()

  # 2. Loop over every discovered sub-collection
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

  # 3. Combine into the exact requested table structure
  if (length(master_list) > 0) {
    final_df <- bind_rows(master_list) |>
      select(Sub_Collection, Authors, Year, Title, DOI, APA_Citation, Abstract)

    # 1. Separate items with a valid DOI from those without
    has_doi  <- final_df |> dplyr::filter(!is.na(DOI) & DOI != "" & DOI != "NA")
    no_doi   <- final_df |> dplyr::filter(is.na(DOI) | DOI == "" | DOI == "NA")

    # 2. Only deduplicate the rows that have a DOI
    deduped_doi <- has_doi |> distinct(DOI, .keep_all = TRUE)

    # 3. Combine them back together
    return(bind_rows(deduped_doi, no_doi))

  } else {
    return(data.frame())
  }
}
