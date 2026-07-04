fetch_collection_items <- function(group_id, api_key, collection_key) {

  all_items <- list()
  start_index <- 0
  has_more <- TRUE
  
  while(has_more) {
    url <- paste0("https://api.zotero.org/groups/", group_id, 
                  "/collections/", collection_key, 
                  "/items/top?limit=100&start=", start_index)
    
    response <- httr::GET(url, httr::add_headers("Zotero-API-Key" = api_key))
    
    if (httr::status_code(response) != 200) {
      warning(paste("Failed to fetch page starting at", start_index, "for collection", collection_key))
      break
    }
    
    json_text <- httr::content(response, "text", encoding = "UTF-8")
    raw_list  <- jsonlite::fromJSON(json_text, simplifyVector = FALSE)
    
    # If the page returned is completely empty, we are finished pagination
    if (length(raw_list) == 0) {
      has_more <- FALSE
      break
    }
    
    # Process this batch using our core formatter
    batch_df <- lapply(raw_list, zotero_to_apa) |> bind_rows()
    all_items[[length(all_items) + 1]] <- batch_df
    
    # Check Zotero's pagination header to see if there's another link page
    link_header <- httr::headers(response)[["link"]]
    if (!is.null(link_header) && grepl('rel="next"', link_header)) {
      start_index <- start_index + 100
    } else {
      has_more <- FALSE
    }
  }
  
  if (length(all_items) > 0) {
    return(bind_rows(all_items))
  } else {
    return(data.frame())
  }
}