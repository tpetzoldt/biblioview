library(shiny)
library(shinyjs)
library(shinydashboard)
library(biblioview)
library(dplyr)
library(DT)

ui <- dashboardPage(
  dashboardHeader(
    # Dynamic title output replacing the static string
    title = uiOutput("dynamic_title"),
    titleWidth = 350
  ),
  dashboardSidebar(
    sidebarMenu(
      div(style = "padding: 15px;",
          textInput("group_id", "Zotero Group ID", value = ""),
          passwordInput("api_key", "API Key", value = ""),

          # Step 0: Scan Folders Initializer
          actionButton("scan_btn", "0. Scan Folders", class = "btn-info w-100"),
          br(), br(),

          # Step 0 Select Box Container (Hidden until Scan completes)
          uiOutput("folder_select_container"),

          # Step 1 Container (Hidden until folders are picked/loaded)
          uiOutput("fetch_ui_container"),
          br(),

          # Step 2 & 3 Containers (Hidden until library data exists)
          uiOutput("enrich_ui_container"),
          br(),
          uiOutput("citation_ui_container"),

          hr(),
          htmlOutput("status_text")
      )
    )
  ),
  dashboardBody(
    useShinyjs(),
    fluidRow(
      box(width = 12, title = "Searchable Reference Database", solidHeader = TRUE, status = "primary",
          DTOutput("bib_table")
      )
    )
  )
)

server <- function(input, output, session) {

  # Reactive tracking vectors
  available_folders <- reactiveVal(NULL)
  current_dataset   <- reactiveVal(NULL)

  # Initialize the reactive tracker for the dynamic title string
  app_title <- reactiveVal("Biblioview Portal")

  # Render the dynamic title in the header bar
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

  # --- URL PARAMETER HANDSHAKE (Isolated for Startup Execution) ---
  observe({
    query <- parseQueryString(session$clientData$url_search)

    if (!is.null(query$group) && !is.null(query$key)) {
      # Use isolate() so updating these elements doesn't disrupt the title rendering loop
      isolate({
        updateTextInput(session, "group_id", value = query$group)
        updateTextInput(session, "api_key", value = query$key)

        if (!is.null(query$title)) {
          app_title(query$title)
        }

        # Fire the scanning logic directly using parameters straight out of the URL
        run_folder_scan(query$group, query$key)
      })
    }
  })

  # --- STEP 0: MANUAL SCAN BUTTON ---
  observeEvent(input$scan_btn, {
    req(input$group_id, input$api_key)
    run_folder_scan(input$group_id, input$api_key)
  })

  # Dynamic Dropdown: Populates when folder keys are loaded
  output$folder_select_container <- renderUI({
    folders <- available_folders()
    if (is.null(folders)) return(NULL)

    tagList(
      selectizeInput("selected_folders", "Folders (Leave blank for all)",
                     choices = folders, multiple = TRUE,
                     options = list(placeholder = 'Select one or more folders')),
      textInput("search_q", "Keyword Search (Optional)", value = ""),
      br()
    )
  })

  # --- STEP 1: DYNAMIC PRIMARY FETCH TRIGGER ---
  output$fetch_ui_container <- renderUI({
    if (is.null(available_folders())) return(NULL)
    actionButton("fetch_btn", "1. Fetch Selected Library", class = "btn-primary w-100")
  })

  # STEP 1 EXECUTION (Updated to pass the vector directly)
  observeEvent(input$fetch_btn, {
    req(input$group_id, input$api_key)

    withProgress(message = 'Retrieving reference entries...', value = 0.5, {

      # If "ROOT" fallback is selected, pass NULL to pull the whole library
      folder_arg <- input$selected_folders
      if ("ROOT" %in% folder_arg) folder_arg <- NULL

      # Single clean call using native pipes
      raw <- fetch_all_zotero_data(
        group_id      = input$group_id,
        api_key       = input$api_key,
        collection_id = folder_arg
      )

      if (input$search_q != "") {
        raw <- raw |> filter(grepl(input$search_q, title, ignore.case = TRUE))
      }

      current_dataset(raw)
    })
  })

  # --- STEP 2 & 3: DYNAMIC CONFIGURATION OVERLAYS ---
  output$enrich_ui_container <- renderUI({
    if (is.null(current_dataset())) return(NULL)
    actionButton("enrich_btn", "2. Run Abstract Enrichment", class = "btn-warning w-100")
  })

  output$citation_ui_container <- renderUI({
    if (is.null(current_dataset())) return(NULL)
    actionButton("citation_btn", "3. Fetch Citation Metrics", class = "btn-success w-100")
  })

  # STEP 2 EXECUTION: Abstract Fill Loop
  observeEvent(input$enrich_btn, {
    df <- current_dataset()
    req(df)
    withProgress(message = 'Filling missing abstract data...', value = 0.5, {
      df <- enrich_missing_abstracts(df)
      current_dataset(df)
    })
  })

  # STEP 3 EXECUTION: Batch Citations
  observeEvent(input$citation_btn, {
    df <- current_dataset()
    req(df)
    withProgress(message = 'Retrieving OpenAlex metrics...', value = 0.5, {
      df <- biblioview::fetch_citation_counts(df)
      current_dataset(df)
    })
  })

  # --- DATA OUTPUT & INTERFACE GENERATION ---
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
      # Count rows that are valid matches and NOT our -1 missing flag
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
