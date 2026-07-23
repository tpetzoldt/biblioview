## Plain version for embedding in an iframe
library(shiny)
library(biblioview)
library(DT)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("

      /* Remove margin/padding for clean iframe fit */
      body, .container-fluid { padding: 0 !important; margin: 0 !important; }
      .dataTables_wrapper { padding: 14px; }

      /* Brilliant Blue Header & High-Contrast White Sort Arrows */
      table.dataTable thead th {
        background-color: #2563eb !important; /* Brilliant Blue */
        color: #ffffff !important;
        font-weight: 600 !important;
        border-bottom: 2px solid #1d4ed8 !important;
        padding: 10px 12px !important;
      }

      /* Force DataTables sort arrows to render in white */
      table.dataTable thead .sorting::before,
      table.dataTable thead .sorting::after,
      table.dataTable thead .sorting_asc::before,
      table.dataTable thead .sorting_asc::after,
      table.dataTable thead .sorting_desc::before,
      table.dataTable thead .sorting_desc::after {
        color: #ffffff !important;
        opacity: 0.8 !important; /* Makes inactive state visible against bright blue */
      }

      /* Make active sort direction 100% white */
      table.dataTable thead .sorting_asc::before,
      table.dataTable thead .sorting_desc::after {
        opacity: 1 !important;
      }

      /* Blue Filter Inputs in Header */
      table.dataTable thead input, table.dataTable thead select {
        border: 1px solid #93c5fd !important;
        border-radius: 4px !important;
        color: #1e293b !important;
      }

      /* Gentle Zebra Striping & Interactive Hover */
      table.dataTable tbody tr:nth-child(even) {
        background-color: #f8fafc !important;
      }
      table.dataTable tbody tr:hover {
        background-color: #e2e8f0 !important;
        transition: background-color 0.15s ease-in-out;
      }
      /* Styled Blue Export Buttons (CSV / Excel / Copy) */
      .dt-button {
        background: #2563eb !important;
        color: #ffffff !important;
        border: none !important;
        border-radius: 6px !important;
        padding: 6px 14px !important;
        font-weight: 500 !important;
        box-shadow: 0 2px 4px rgba(37, 99, 235, 0.2) !important;
        margin-right: 6px !important;
      }
      .dt-button:hover {
        background: #1d4ed8 !important;
        box-shadow: 0 4px 6px rgba(29, 78, 216, 0.3) !important;
      }

      /* Clean Search Box and Pagination Styling */
      .dataTables_filter input, .dataTables_length select {
        border: 1px solid #cbd5e1 !important;
        border-radius: 6px !important;
        padding: 4px 8px !important;
      }
    "))
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
