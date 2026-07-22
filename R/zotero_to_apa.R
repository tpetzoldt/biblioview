#' Convert Zotero Item Data to APA Citation Format
#'
#' Parses a raw Zotero JSON item object, extracts metadata fields safely, clears
#' malformed DOI formatting, and constructs a standardized APA citation string.
#'
#' @param item List. A single raw item element unpacked from the Zotero JSON response.
#'
#' @return A single-row data frame with standardized bibliographic columns.
#' @export
zotero_to_apa <- function(item) {
  d <- item$data
  type <- if(!is.null(d$itemType)) d$itemType else "journalArticle"

  # Helper function to guarantee an absolute length of 1 for dataframe building
  safe_field <- function(field, default = "") {
    if (is.null(field) || length(field) == 0) return(default)
    # If it's an array/list, collapse it down to a single text block
    if (length(field) > 1) return(paste(unlist(field), collapse = " "))
    return(as.character(field))
  }

  # Format Authors safely
  authors_apa <- "Unknown"
  if (!is.null(d$creators) && length(d$creators) > 0) {
    formatted_creators <- sapply(d$creators, function(c) {
      if (!is.null(c$lastName) && length(c$lastName) > 0 && c$lastName != "") {
        # Standard individual: Last Name, Initials
        if (!is.null(c$firstName) && length(c$firstName) > 0 && c$firstName != "") {
          initials <- paste0(substr(strsplit(as.character(c$firstName), " ")[[1]], 1, 1), ".", collapse = " ")
          return(paste0(c$lastName, ", ", initials))
        } else {
          return(as.character(c$lastName))
        }
      } else if (!is.null(c$name) && length(c$name) > 0 && c$name != "") {
        # Institutional / Corporate author (e.g., R Core Team)
        return(as.character(c$name))
      }
      return(NULL)
    })

    # Filter out any NULL elements before working with the vector
    formatted_creators <- unlist(formatted_creators)

    if(length(formatted_creators) > 1) {
      authors_apa <- paste(paste(head(formatted_creators, -1), collapse = ", "),
                           tail(formatted_creators, 1), sep = ", & ")
    } else if (length(formatted_creators) == 1) {
      authors_apa <- formatted_creators
    }
  }

  # Extract values using the safe_field wrapper to guarantee length = 1
  year     <- if(!is.null(d$date) && length(d$date) > 0) substr(d$date, 1, 4) else "n.d."
  title    <- safe_field(d$title, default = "Untitled")
  abstract <- safe_field(d$abstractNote, default = "")
  extra    <- safe_field(d$extra, default = "") # <-- 1. Extract raw extra field cleanly
  doi      <- safe_field(d$DOI, default = "")
  url_link <- safe_field(d$url, default = "")

  journal  <- safe_field(d$publicationTitle, default = "")
  volume   <- safe_field(d$volume, default = "")
  issue    <- safe_field(d$issue, default = "")
  pages    <- safe_field(d$pages, default = "")
  pub      <- safe_field(d$publisher, default = "")

  source_str <- ""
  if (type == "journalArticle" && journal != "") {
    source_str <- paste0("*", journal, "*")
    if (volume != "") source_str <- paste0(source_str, ", *", volume, "*")
    if (issue != "")  source_str <- paste0(source_str, "(", issue, ")")
    if (pages != "")  source_str <- paste0(source_str, ", ", pages)
  } else if ((type == "book" || type == "bookSection") && pub != "") {
    source_str <- pub
  }

  # 1. Clean the raw DOI field if it contains accidental URL prefixes
  if (doi != "") {
    doi <- sub("^.*?10\\.", "10.", doi)
  }

  # 2. Build uniform hyperlink fallback
  doi_str <- if(doi != "") paste0("https://doi.org/", doi) else url_link

  apa_citation <- paste0(authors_apa, " (", year, "). ", title, ". ", source_str)
  if(doi_str != "") apa_citation <- paste0(apa_citation, " ", doi_str)

  # Build using explicit, robust assignments
  out <- data.frame(
    Authors          = safe_field(authors_apa, "Unknown"),
    Year             = safe_field(year, "n.d."),
    Title            = title,
    DOI              = doi_str,
    APA_Citation     = safe_field(apa_citation, "Untitled Reference"),
    Abstract         = abstract,
    extra            = extra, # <-- 2. Return extra in the single-row data frame
    stringsAsFactors = FALSE
  )

  return(out)
}
