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
    width = 300,
    sidebarMenu(
      div(style = "padding: 15px;",
          textInput("group_id", "Zotero Group ID", value = ""),
          passwordInput("api_key", "API Key", value = ""),

          # Step 0: Scan Folders
          actionButton("scan_btn", "0. Scan Folders", class = "btn-warning w-100"),
          br(), br(),

          uiOutput("folder_select_container"),

          uiOutput("fetch_ui_container"),
          br(),

          # Steps 1 and 2
          uiOutput("citation_ui_container"),
          br(),
          uiOutput("enrich_ui_container"),

          hr(),

          # Dynamic API Politeness Input Panel
          # Only displays after initial setup is verified to avoid cluttering the login
          uiOutput("polite_email_container"),

          htmlOutput("status_text")
      ),
      # Lower persistent acknowledgement block
      div(class = "sidebar-acknowledgments",
          hr(style = "border-color: #4b646f; margin-bottom: 10px;"),
          # Changed from p() to div() with explicit wrapping rules to guarantee text folds correctly
          div("Powered by open scholarly infrastructure. Data retrieved and enriched via standard APIs from:",
              style = "font-size: 0.85em; color: #8a979e; margin-bottom: 5px; white-space: normal; word-wrap: break-word; line-height: 1.3;"),
          tags$ul(style = "font-size: 0.85em; color: #b8c7ce; padding-left: 15px; margin-bottom: 10px;",
                  tags$li(
                    tags$a(href = "https://www.zotero.org", target = "_blank", "Zotero", style = "color: #b8c7ce; text-decoration: underline;"),
                    " (Group Libraries)"
                  ),
                  tags$li(
                    tags$a(href = "https://www.crossref.org", target = "_blank", "Crossref", style = "color: #b8c7ce; text-decoration: underline;"),
                    " (Metadata & Abstracts)"
                  ),
                  tags$li(
                    tags$a(href = "https://openalex.org", target = "_blank", "OpenAlex", style = "color: #b8c7ce; text-decoration: underline;"),
                    " (Abstracts and Citation Metrics)"
                  ),
                  tags$li(
                    tags$a(href = "https://europepmc.org", target = "_blank", "Europe PMC", style = "color: #b8c7ce; text-decoration: underline;"),
                    " (Open Life Science Index)"
                  )
          ),
          div(style = "font-size: 0.8em; color: #8a979e; display: flex; justify-content: space-between; padding: 0 5px;",
              span(paste("v0.1.0 |", format(Sys.Date(), "%Y-%m-%d"))),
              tags$a(href = "https://github.com/tpetzoldt/biblioview", target = "_blank",
                     style ="color: #3d8d8d; text-decoration: underline;",
                     tags$i(class = "fa fa-github", style = "color: #3d8d8d; text-decoration: underline;"), "https://github.com/tpetzoldt")
          )
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .dataTable tbody td {
          vertical-align: top !important;
        }
        .main-sidebar { width: 300px !important; }
        .content-wrapper, .main-footer, .right-side { margin-left: 300px !important; }
        /* Style adjustments for the small helper text under the email input */
        .help-block-polite {
          font-size: 0.85em;
          color: #b8c7ce;
          margin-top: 5px;
          line-height: 1.3;
        }
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

  # --- UNIFIED LAUNCH PARAMETER HANDSHAKE ---
  observe({
    # 1. Capture URL query strings (Primary for Shiny Server deployments)
    query <- parseQueryString(session$clientData$url_search)

    # 2. Capture R global options (Primary for local launch_dashboard() function)
    opt_group <- getOption("biblioview.group", default = "")
    opt_key   <- getOption("biblioview.key",   default = "")
    opt_title <- getOption("biblioview.title", default = "")

    # 3. Resolve prioritization: URL parameters take precedence over function arguments
    final_group <- if (!is.null(query$group)) query$group else opt_group
    final_key   <- if (!is.null(query$key))   query$key   else opt_key
    final_title <- if (!is.null(query$title)) query$title else opt_title

    # 4. If credentials exist via either method, execute the scan instantly
    if (final_group != "" && final_key != "") {
      isolate({
        updateTextInput(session, "group_id", value = final_group)
        updateTextInput(session, "api_key", value = final_key)

        if (final_title != "") {
          app_title(final_title)
        }

        run_folder_scan(final_group, final_key)
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

  # --- DYNAMIC CONTROLS DISPLAY CONTAINER ---
  output$citation_ui_container <- renderUI({
    if (is.null(current_dataset())) return(NULL)
    actionButton("citation_btn", "2. Fetch Citation Metrics", class = "btn-warning w-100")
  })

  output$enrich_ui_container <- renderUI({
    if (is.null(current_dataset())) return(NULL)
    actionButton("enrich_btn", "3. Run Abstract Enrichment", class = "btn-warning w-100")
  })

  output$polite_email_container <- renderUI({
    if (is.null(current_dataset())) return(NULL)

    default_email <- Sys.getenv("POLITE_EMAIL")

    tagList(
      textInput("polite_email", "API Contact Email (Optional)", value = default_email),
      # Added explicit wrapping rules directly onto the list element
      tags$ul(class = "help-block-polite",
              style = "padding-left: 15px; margin-top: 5px; white-space: normal; word-wrap: break-word; line-height: 1.3;",
              tags$li("Providing a valid email address is polite open-access etiquette."),
              tags$li("Grants your requests access to the higher-priority OpenAlex Polite Pool.")
      ),
      br()
    )
  })

  # --- STEP 2 EXECUTION: BATCH CITATIONS
  observeEvent(input$citation_btn, {
    df <- current_dataset()
    req(df)

    # 1. Pull the raw UI string (or default to empty if the UI component isn't rendered yet)
    ui_email <- if (!is.null(input$polite_email)) trimws(input$polite_email) else ""

    # 2. Fallback cascade: Use UI string if filled; otherwise drop back to system environment
    user_email <- if (ui_email != "") ui_email else Sys.getenv("POLITE_EMAIL")

    withProgress(message = 'Retrieving OpenAlex metrics...', value = 0.5, {
      # Passes the safely resolved email down into the package logic
      df <- biblioview::fetch_citation_counts(df, email = user_email)
      current_dataset(df)
    })
  })

  # --- STEP 3 EXECUTION: MODAL INTERCEPT & ABSTRACT ENRICHMENT ---
  # Intercept the primary click to spawn the safety dialog box layout
  observeEvent(input$enrich_btn, {
    req(current_dataset())

    showModal(modalDialog(
      title = "Confirm Abstract Enrichment Operation",
      span("Retrieving missing abstracts systematically queries external metadata endpoints item-by-item. This operational loop takes time and consumes shared server network resources."),
      br(), br(),
      strong("Do you really want to proceed with enrichment?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_enrich_btn", "Yes, Proceed", class = "btn-warning")
      ),
      size = "m",
      easyClose = TRUE
    ))
  })

  # Actual execution trigger linked inside the modal confirmation action handle
  observeEvent(input$confirm_enrich_btn, {
    removeModal() # Clear the overlay box away immediately
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
      return("Folders scanned. Ready to fetch library")
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
