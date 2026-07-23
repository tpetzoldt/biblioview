## Plain version for embedding in an iframe
library(shiny)
library(biblioview)
library(DT)
library(memoise)

# ==============================================================================
# GLOBAL MEMOISED FETCH FUNCTIONS (Shared across all user sessions)
# ==============================================================================

# 1. Cache collection definitions (folder name -> key lookup) for 5 minutes
fetch_collections_cached <- memoise(
  function(group, key) {
    coll_url <- paste0("https://api.zotero.org/groups/", group, "/collections")
    coll_res <- httr::GET(coll_url, httr::add_headers("Zotero-API-Key" = key))

    if (httr::status_code(coll_res) == 200) {
      jsonlite::fromJSON(
        httr::content(coll_res, "text", encoding = "UTF-8"),
        simplifyVector = FALSE
      )
    } else {
      list()
    }
  },
  cache = cachem::cache_mem(max_age = 300) # 300 seconds = 5 minutes
)

# 2. Cache main bibliographic item dataset for 5 minutes
fetch_zotero_cached <- memoise(
  function(group, key, collection_id) {
    fetch_all_zotero_data(
      group_id      = group,
      api_key       = key,
      collection_id = collection_id
    )
  },
  cache = cachem::cache_mem(max_age = 300) # 300 seconds = 5 minutes
)

# ==============================================================================
# SHINY UI & SERVER logic
# ==============================================================================

ui <- fluidPage(
  tags$head(
    includeCSS("www/custom.css")
  ),
  uiOutput("embed_ui")
)

server <- function(input, output, session) {

  `%||%` <- function(a, b) if (!is.null(a) && nzchar(a)) a else b

  url_params <- reactive({
    query <- getQueryString()

    list(
      group  = query$group  %||% getOption("biblioview.group", ""),
      key    = query$key    %||% getOption("biblioview.key", ""),
      folder = query$folder %||% getOption("biblioview.folder", NULL)
    )
  })

  dataset <- reactive({
    params <- url_params()

    req(params$group != "", params$key != "")

    target_collection_id <- params$folder

    # If a folder parameter is provided, resolve name to Key via cached lookup
    if (!is.null(target_collection_id)) {
      all_colls <- fetch_collections_cached(params$group, params$key)

      if (length(all_colls) > 0) {
        matched_name <- Filter(function(x) {
          tolower(x$data$name) == tolower(target_collection_id)
        }, all_colls)

        if (length(matched_name) > 0) {
          target_collection_id <- matched_name[[1]]$key
        }
      }
    }

    # Fetch data using the memoised function
    fetch_zotero_cached(
      group         = params$group,
      key           = params$key,
      collection_id = target_collection_id
    )
  })

  output$embed_ui <- renderUI({
    params <- url_params()

    if (params$group == "" || params$key == "") {
      return(
        div(class = "alert alert-warning",
            h4("Missing Launch Parameters"),
            p("Please supply group and key via URL parameters or options."),
            tags$code("?group=12345&key=ABCDE&folder=COLLECTION_KEY")
        )
      )
    }

    DTOutput("embed_table")
  })

  output$embed_table <- renderDT({
    df <- dataset()
    req(df)

    # Drop unwanted columns for the plain embed view
    slim_df <- df |>
      dplyr::select(c("Authors", "Year", "Title", "APA_Citation", "DOI"))

    render_biblioview_table(slim_df, show_buttons = TRUE)
  })
}

shinyApp(ui, server)
