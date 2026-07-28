#' Clean and Sanitize Abstract Text
#'
#' Strips or converts HTML/XML markup (like sub/sup tags and JATS math)
#' into clean plain text for Zotero notes.
#'
#' @param text Character string containing raw abstract text.
#' @return Cleaned character string.
sanitize_abstract_text <- function(text) {
  if (is.null(text) || !nzchar(text)) return(NULL)

  clean <- text

  # 1. Standardize line breaks and double spaces
  clean <- gsub("\r\n|\r", "\n", clean)
  clean <- gsub("&nbsp;", " ", clean, fixed = TRUE)

  # 2. Handle XML/JATS math & formula tags before stripping HTML
  # Replace common JATS tags with standard inline text equivalents
  clean <- gsub("<inline-formula>.*?</inline-formula>", "[formula]", clean, ignore.case = TRUE)
  clean <- gsub("</?sub-script>", "", clean, ignore.case = TRUE)
  clean <- gsub("</?super-script>", "", clean, ignore.case = TRUE)

  # 3. Convert sub/sup tags to plain text (e.g., H<sub>2</sub>O -> H2O)
  # If you prefer keeping HTML formatting, Zotero *does* support <sub> and <sup>!
  # But if they are broken, stripping or replacing them prevents raw tag clutter.
  clean <- gsub("<sub>(.*?)</sub>", "\\1", clean, ignore.case = TRUE)
  clean <- gsub("<sup>(.*?)</sup>", "\\1", clean, ignore.case = TRUE)

  # 4. Strip any remaining HTML/XML tags safely using rvest/xml2 if available,
  # or fallback to regex if not.
  if (requireNamespace("rvest", quietly = TRUE)) {
    clean <- tryCatch({
      clean |>
        xml2::read_html() |>
        rvest::html_text2()
    }, error = function(e) {
      # Fallback regex tag stripping if XML parsing fails on malformed fragments
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
