## Plain version for embedding in an iframe
library(shiny)
library(biblioview)
library(DT)

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

    # If a folder parameter is provided, check if it's a folder Name instead of a Key
    if (!is.null(target_collection_id)) {
      # Fetch all collections definitions once to build a lookup map
      coll_url <- paste0("https://api.zotero.org/groups/", params$group, "/collections")
      coll_res <- httr::GET(coll_url, httr::add_headers("Zotero-API-Key" = params$key))

      if (httr::status_code(coll_res) == 200) {
        all_colls <- jsonlite::fromJSON(
          httr::content(coll_res, "text", encoding = "UTF-8"),
          simplifyVector = FALSE
        )

        # Check if target_collection_id matches a folder NAME (case-insensitive)
        matched_name <- Filter(function(x) {
          tolower(x$data$name) == tolower(target_collection_id)
        }, all_colls)

        # If matched by name, extract its alphanumeric key!
        if (length(matched_name) > 0) {
          target_collection_id <- matched_name[[1]]$key
        }
      }
    }

    # Fetch data using the resolved collection key
    fetch_all_zotero_data(
      group_id      = params$group,
      api_key       = params$key,
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
      #dplyr::select(-any_of(c("Sub_Collection", "extra", "Abstract"))) |>
      dplyr::select(c("Authors", "Year", "Title", "APA_Citation", "DOI"))

    render_biblioview_table(slim_df, show_buttons = TRUE)
  })
}

shinyApp(ui, server)
