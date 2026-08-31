# Extended version allowing ancient dates BC / BCE; not yet tested
#
#' Extract Year from Zotero Date Fields (including BCE / Ancient Dates)
#'
#' @param date_vec Character vector of raw date strings from Zotero.
#' @return Character vector containing year strings (e.g., "2024", "384 BCE", "-0383", or NA).
extract_zotero_year_extended <- function(date_vec) {
  if (is.null(date_vec) || length(date_vec) == 0) return(character(0))

  date_str <- as.character(date_vec)

  # 1. Match explicit BCE/BC dates (e.g., "384 BCE", "c. 384 BC")
  bce_pattern <- "\\b(\\d{1,4})\\s*(BCE|BC)\\b"

  # 2. Match standard CE years (1 to 4 digits: 300 to 2099)
  ce_pattern <- "\\b([1-9]\\d{0,3}|20\\d{2})\\b"

  years <- rep(NA_character_, length(date_str))

  for (i in seq_along(date_str)) {
    val <- date_str[i]
    if (is.na(val) || val == "") next

    if (grepl(bce_pattern, val, ignore.case = TRUE)) {
      # Retain BCE suffix for clear display in app
      years[i] <- sub(paste0(".*?", bce_pattern, ".*"), "\\1 BCE", val, ignore.case = TRUE)
    } else if (grepl(ce_pattern, val)) {
      years[i] <- sub(paste0(".*?", ce_pattern, ".*"), "\\1", val)
    }
  }

  return(years)
}
