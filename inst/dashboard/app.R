library(shiny)
library(shinydashboard)
library(biblioview)
library(dplyr)
library(DT)

ui <- dashboardPage(
  dashboardHeader(
    title = uiOutput("dynamic_title"),
    titleWidth = 350
  ),
  dashboardSidebar(
    width = 300, # Widened sidebar to fit labels beautifully
    sidebarMenu(
      div(style = "padding: 15px;",
          textInput("group_id", "Zotero Group ID", value = ""),
          passwordInput("api_key", "API Key", value = ""),

          # Step 0: Scan Folders (Unified style & full width)
          actionButton("scan_btn", "0. Scan Folders", class = "btn-warning w-100"),
          br(), br(),

          uiOutput("folder_select_container"),

          uiOutput("fetch_ui_container"),
          br(),

          # Steps 1 and 2 flipped in order here
          uiOutput("citation_ui_container"),
          br(),
          uiOutput("enrich_ui_container"),

          hr(),
          htmlOutput("status_text")
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .dataTable tbody td {
          vertical-align: top !important;
        }
        /* Ensures the sidebar width configuration applies properly across the dashboard layout */
        .main-sidebar { width: 300px !important; }
        .content-wrapper, .main-footer, .right-side { margin-left: 300px !important; }
      "))
    ),
    fluidRow(
      box(width = 12, title = "Searchable Reference Database", solidHeader = TRUE, status = "primary",
          DTOutput("bib_table")
      )
    )
  )
)

server <- function(input, output, session) {

  available_folders <- reactiveVal(NULL)
  current_dataset   <- reactiveVal(NULL)
  app_title         <- reactiveVal("Biblioview Portal")

  output$dynamic_title <- renderUI({
    tags$span(app_title())
  })

  # --- CENTRALIZED SCAN FUNCTION ---
  run_folder_scan <- function(target_group, target_key) {
    withProgress(message = 'Scanning group folders...', value = 0.5, {
      folders <- biblioview::fetch_zotero_collections(target_group, target_key)

      if (length(folders) == 0) {
        showNotification("No sub-folders found or invalid credentials. Showing root library by default.", type = "warning")
        available_folders(c("All Folders (Root)" = "ROOT"))
      } else {
        available_folders(folders)
      }
    })
  }

  # --- URL PARAMETER HANDSHAKE ---
  observe({
    query <- parseQueryString(session$clientData$url_search)

    if (!is.null(query$group) && !is.null(query$key)) {
      isolate({
        updateTextInput(session, "group_id", value = query$group)
        updateTextInput(session, "api_key", value = query$key)

        if (!is.null(query$title)) {
          app_title(query$title)
        }

        run_folder_scan(query$group, query$key)
      })
    }
  })

  # --- STEP 0: MANUAL SCAN BUTTON ---
  observeEvent(input$scan_btn, {
    req(input$group_id, input$api_key)
    run_folder_scan(input$group_id, input$api_key)
  })

  # Dynamic Dropdown
  output$folder_select_container <- renderUI({
    folders <- available_folders()
    if (is.null(folders)) return(NULL)

    tagList(
      selectizeInput("selected_folders", "Folders (Leave blank for all)",
                     choices = folders, multiple = TRUE,
                     options = list(placeholder = 'Select one or more folders')),
      br()
    )
  })

  # --- STEP 1: FETCH LIBRARY ---
  output$fetch_ui_container <- renderUI({
    if (is.null(available_folders())) return(NULL)
    actionButton("fetch_btn", "1. Fetch Selected Library", class = "btn-warning w-100")
  })

  observeEvent(input$fetch_btn, {
    req(input$group_id, input$api_key)

    withProgress(message = 'Retrieving reference entries...', value = 0.5, {
      folder_arg <- input$selected_folders
      if ("ROOT" %in% folder_arg) folder_arg = NULL

      raw <- fetch_all_zotero_data(
        group_id      = input$group_id,
        api_key       = input$api_key,
        collection_id = folder_arg
      )

      current_dataset(raw)
    })
  })

  # --- STEP 2 & 3: ENRICHMENT OVERLAYS (Flipped Order & Matched Colors) ---
  output$citation_ui_container <- renderUI({
    if (is.null(current_dataset())) return(NULL)
    actionButton("citation_btn", "2. Fetch Citation Metrics", class = "btn-warning w-100")
  })

  output$enrich_ui_container <- renderUI({
    if (is.null(current_dataset())) return(NULL)
    actionButton("enrich_btn", "3. Run Abstract Enrichment", class = "btn-warning w-100")
  })

  observeEvent(input$citation_btn, {
    df <- current_dataset()
    req(df)
    withProgress(message = 'Retrieving OpenAlex metrics...', value = 0.5, {
      df <- biblioview::fetch_citation_counts(df)
      current_dataset(df)
    })
  })

  observeEvent(input$enrich_btn, {
    df <- current_dataset()
    req(df)
    withProgress(message = 'Filling missing abstract data...', value = 0.5, {
      df <- enrich_missing_abstracts(df)
      current_dataset(df)
    })
  })

  # --- DATA OUTPUT ---
  output$bib_table <- renderDT({
    df <- current_dataset()
    req(df)

    datatable(
      biblioview::format_hyperlinks(df),
      escape = FALSE,
      extensions = 'Buttons',
      options = list(
        dom = 'Blfrtip',
        buttons = c('copy', 'csv', 'excel'),
        pageLength = 15,
        lengthMenu = list(c(10, 15, 20, 50, 100, 200, -1), c('10', '15', '20', '50', '100', '200', 'All'))
      )
    )
  })

  output$status_text <- renderUI({
    df <- current_dataset()
    if (is.null(df)) {
      if (is.null(available_folders())) return("Ready to scan library configuration.")
      return("Folders mapped. Ready to fetch records.")
    }

    status_msg <- paste0("Loaded ", nrow(df), " entries successfully.")

    if ("citations" %in% names(df)) {
      valid_counts <- sum(!is.na(df$citations) & df$citations != -1)
      tagList(
        status_msg,
        br(),
        paste0("Citations mapped for ", valid_counts, " items."),
        br(),
        span(style = "font-style: italic;", "-1: no citation count available")
      )
    } else {
      status_msg
    }
  })
}

shinyApp(ui, server)
