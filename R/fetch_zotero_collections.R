#' Fetch Sub-Folders (Collections) from a Zotero Group
#'
#' @description
#' Connects to the Zotero API and retrieves a named vector of all sub-folders
#' (collections) within the specified group library.
#'
#' @param group_id Character or numeric string containing the Zotero Group ID.
#' @param api_key Character string containing the Zotero API access key.
#'
#' @return A named character vector where the values are the unique Zotero
#'   collection hashes (e.g., `"A8F3X2Z9"`) and the names are the human-readable
#'   folder titles (e.g., `"Marine Biology"`). Returns an empty vector if no
#'   folders exist.
#' @export
#'
#' @importFrom httr GET content add_headers
#'
#' @examples
#' \dontrun{
#' folders <- fetch_zotero_collections("1234567", "your_api_key")
#' }
fetch_zotero_collections <- function(group_id, api_key) {
  if (missing(group_id) || missing(api_key) || group_id == "" || api_key == "") {
    stop("Both group_id and api_key must be provided.")
  }

  url <- paste0("https://api.zotero.org/groups/", group_id, "/collections")

  tryCatch({
    resp <- httr::GET(
      url = url,
      httr::add_headers("Zotero-API-Key" = api_key)
    )

    if (resp$status_code != 200) {
      warning("Failed to connect to Zotero API. Check credentials.")
      return(character(0))
    }

    body <- httr::content(resp, as = "parsed", type = "application/json")

    if (length(body) == 0) return(character(0))

    # Extract unique collection keys
    folders <- sapply(body, function(x) x$key)

    # FIX: Zotero stores the folder names inside x$data$name, not x$meta$name
    names(folders) <- sapply(body, function(x) {
      if (!is.null(x$data$name)) x$data$name else x$key
    })

    return(folders)
  }, error = function(e) {
    warning("Error scanning Zotero collections: ", e$message)
    return(character(0))
  })
}
