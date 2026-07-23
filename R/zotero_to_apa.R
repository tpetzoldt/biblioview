#' Convert Zotero Item Data to Simplified Plain-Text APA 7 Format
#'
#' Parses Zotero JSON items across articles, books, chapters, reports, and
#' webpages, enforcing APA 7th rules without rich text (no italics or bolding).
#'
#' @param item List. A single raw item element from the Zotero JSON API.
#' @return A single-row data frame with standardized bibliographic columns.
#' @export
zotero_to_apa <- function(item) {
  d <- item$data
  type <- if (!is.null(d$itemType)) d$itemType else "journalArticle"

  # Helper function to guarantee length 1 output
  safe_field <- function(field, default = "") {
    if (is.null(field) || length(field) == 0) return(default)
    if (length(field) > 1) return(paste(unlist(field), collapse = " "))
    return(as.character(field))
  }

  # --------------------------------------------------------------------------
  # 1. Author / Creator Formatting (APA 7 Rules)
  # --------------------------------------------------------------------------
  authors_apa <- "Unknown"
  if (!is.null(d$creators) && length(d$creators) > 0) {
    valid_creators <- Filter(function(c) {
      role <- if (!is.null(c$creatorType)) c$creatorType else "author"
      role %in% c("author", "editor", "director")
    }, d$creators)

    if (length(valid_creators) == 0) valid_creators <- d$creators

    formatted_creators <- sapply(valid_creators, function(c) {
      if (!is.null(c$lastName) && length(c$lastName) > 0 && c$lastName != "") {
        if (!is.null(c$firstName) && length(c$firstName) > 0 && c$firstName != "") {
          first_parts <- strsplit(trimws(as.character(c$firstName)), "[ -]")[[1]]
          initials <- paste0(substr(first_parts, 1, 1), ".", collapse = " ")
          return(paste0(c$lastName, ", ", initials))
        } else {
          return(as.character(c$lastName))
        }
      } else if (!is.null(c$name) && length(c$name) > 0 && c$name != "") {
        return(as.character(c$name))
      }
      return(NULL)
    })

    formatted_creators <- unlist(formatted_creators[!sapply(formatted_creators, is.null)])
    n_auth <- length(formatted_creators)

    if (n_auth == 1) {
      authors_apa <- formatted_creators[1]
    } else if (n_auth == 2) {
      authors_apa <- paste(formatted_creators[1], formatted_creators[2], sep = " & ")
    } else if (n_auth > 2 && n_auth <= 20) {
      authors_apa <- paste(
        paste(formatted_creators[1:(n_auth - 1)], collapse = ", "),
        formatted_creators[n_auth],
        sep = ", & "
      )
    } else if (n_auth > 20) {
      authors_apa <- paste0(
        paste(formatted_creators[1:19], collapse = ", "),
        ", . . . ",
        formatted_creators[n_auth]
      )
    }
  }

  # --------------------------------------------------------------------------
  # 2. Extract Metadata Fields
  # --------------------------------------------------------------------------
  year     <- if (!is.null(d$date) && length(d$date) > 0) substr(d$date, 1, 4) else "n.d."
  title    <- safe_field(d$title, default = "Untitled")
  abstract <- safe_field(d$abstractNote, default = "")
  extra    <- safe_field(d$extra, default = "")
  doi      <- safe_field(d$DOI, default = "")
  url_link <- safe_field(d$url, default = "")

  journal     <- safe_field(d$publicationTitle, default = "")
  book_title  <- safe_field(d$bookTitle, default = "")
  volume      <- safe_field(d$volume, default = "")
  issue       <- safe_field(d$issue, default = "")
  pages       <- safe_field(d$pages, default = "")
  pub         <- safe_field(d$publisher, default = "")
  institution <- safe_field(d$institution, default = "")

  title_clean <- sub("\\.$", "", title)

  # --------------------------------------------------------------------------
  # 3. Build Plain-Text Source Strings & Titles
  # --------------------------------------------------------------------------
  formatted_title <- title_clean
  source_str <- ""

  if (type == "journalArticle") {
    if (journal != "") {
      source_str <- journal
      if (volume != "") source_str <- paste0(source_str, ", ", volume)
      if (issue != "")  source_str <- paste0(source_str, "(", issue, ")")
      if (pages != "")  source_str <- paste0(source_str, ", ", pages)
    }

  } else if (type == "book") {
    if (pub != "") source_str <- pub

  } else if (type == "bookSection") {
    if (book_title != "") {
      source_str <- paste0("In ", book_title)
      if (pages != "") source_str <- paste0(source_str, " (pp. ", pages, ")")
      if (pub != "")   source_str <- paste0(source_str, ". ", pub)
    } else if (pub != "") {
      source_str <- pub
    }

  } else if (type == "report") {
    if (institution != "") source_str <- institution else if (pub != "") source_str <- pub

  } else {
    source_str <- if (journal != "") journal else pub
  }

  # --------------------------------------------------------------------------
  # 4. Construct Final Citation String & Links
  # --------------------------------------------------------------------------
  if (doi != "") {
    doi <- sub("^.*?10\\.", "10.", doi)
  }

  doi_str <- if (doi != "") paste0("https://doi.org/", doi) else url_link

  apa_citation <- paste0(authors_apa, " (", year, "). ", formatted_title, ".")
  if (source_str != "") apa_citation <- paste0(apa_citation, " ", source_str, ".")
  if (doi_str != "")    apa_citation <- paste0(apa_citation, " ", doi_str)

  # --------------------------------------------------------------------------
  # 5. Return Data Frame
  # --------------------------------------------------------------------------
  out <- data.frame(
    Authors      = safe_field(authors_apa, "Unknown"),
    Year         = safe_field(year, "n.d."),
    Title        = title,
    DOI          = doi_str,
    APA_Citation = safe_field(apa_citation, "Untitled Reference"),
    Abstract     = abstract,
    extra        = extra,
    stringsAsFactors = FALSE
  )

  return(out)
}
