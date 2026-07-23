## Plain version for embedding in an iframe
library(shiny)
library(biblioview)
library(DT)

ui <- fluidPage(
  # Clean CSS reset for embed
  tags$head(
    tags$style(HTML("
      body, .container-fluid { padding: 0 !important; margin: 0 !important; }
      .dataTables_wrapper { padding: 12px; }
      .alert { margin: 15px; }
    "))
  ),

  # Dynamic UI: Shows error alert if params missing, or table if valid
  uiOutput("embed_ui")
)

server <- function(input, output, session) {

  # Parse URL parameters safely
  url_params <- reactive({
    # Shiny's built-in helper
    query <- getQueryString()

    # Fallback to getOption if URL parameter not provided
    list(
      group   = query$group   %||% getOption("biblioview.group", ""),
      key     = query$key     %||% getOption("biblioview.key", ""),
      folder  = query$folder  %||% getOption("biblioview.folder", NULL),
      keyword = query$keyword %||% NULL
    )
  })

  # Helper %||% operator for NULL fallback
  `%||%` <- function(a, b) if (!is.null(a) && nzchar(a)) a else b

  # Fetch dataset
  dataset <- reactive({
    params <- url_params()

    # Require minimal authorization params to proceed
    req(params$group != "", params$key != "")

    raw <- fetch_all_zotero_data(
      group_id      = params$group,
      api_key       = params$key,
      collection_id = params$folder
    )

    # Optional keyword filter if passed via URL (?keyword=item)
    if (!is.null(params$keyword) && "title" %in% names(raw)) {
      raw <- raw[grep(params$keyword, raw$title, ignore.case = TRUE), ]
    }

    raw
  })

  # Main UI switcher
  output$embed_ui <- renderUI({
    params <- url_params()

    # Diagnostic safety check if API params aren't set
    if (params$group == "" || params$key == "") {
      return(
        div(class = "alert alert-warning",
            h4("Missing Launch Parameters"),
            p("Please supply group and key via URL parameters or options."),
            tags$code("?group=12345&key=ABCDE&keyword=item")
        )
      )
    }

    DTOutput("embed_table")
  })

  output$embed_table <- renderDT({
    df <- dataset()
    req(df)

    # Use package helper function
    render_biblioview_table(df, show_buttons = FALSE)
  })
}

shinyApp(ui, server)
