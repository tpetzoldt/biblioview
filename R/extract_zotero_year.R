#' Extract 4-Digit Year from Raw Zotero Date Fields safely
#'
#' @param date_vec Character vector or list item containing raw date string(s).
#' @return Character vector of 4-digit years (or NA_character_).
#' @export
extract_zotero_year <- function(date_vec) {
  # If an item/closure or non-character object was passed in via lapply
  if (is.function(date_vec)) return(NA_character_)
  if (is.list(date_vec)) {
    # If a list element was passed, try extracting 'date' or first element
    date_vec <- if (!is.null(date_vec$date)) date_vec$date else unlist(date_vec)
  }

  if (is.null(date_vec) || length(date_vec) == 0) return(NA_character_)

  date_str <- as.character(date_vec)

  pattern <- "\\b(1[0-9]{3}|20[0-9]{2})\\b"

  has_year <- grepl(pattern, date_str)

  years <- sub(paste0(".*?", pattern, ".*"), "\\1", date_str)
  years[!has_year | is.na(date_str)] <- NA_character_

  return(years)
}
