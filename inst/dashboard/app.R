library(shinydashboard)
library(biblioview)
library(dplyr)
library(DT)

ui <- dashboardPage(
  title = "Biblioview Portal",
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
          uiOutput("polite_email_container"),

          htmlOutput("status_text")
      ),
      # Lower persistent acknowledgement block
      div(class = "sidebar-acknowledgments",
          hr(style = "border-color: #4b646f; margin-bottom: 10px;"),
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
              span(paste("v0.1.1 |", format(Sys.Date(), "%Y-%m-%d"))),
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
        .help-block-polite {
          font-size: 0.85em;
          color: #b8c7ce;
          margin-top: 5px;
          line-height: 1.3;
        }
        .main-sidebar {
          width: 300px !important;
        }
        .content-wrapper, .main-footer, .right-side {
          margin-left: 300px !important;
        }
        .sidebar-collapse .main-sidebar {
          transform: translate(-300px, 0) !important;
        }
        .sidebar-collapse .content-wrapper,
        .sidebar-collapse .main-footer,
        .sidebar-collapse .right-side,
        .sidebar-collapse .main-header .navbar {
          margin-left: 0px !important;
        }
        .main-sidebar, .content-wrapper, .main-footer, .right-side, .main-header .navbar {
          transition: transform 0.25s ease-in-out, margin-left 0.25s ease-in-out !important;
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
        # --- ALPHABETICAL SORTING ENGINE ---
        if (!is.null(names(folders))) {
          sorted_folders <- folders[order(names(folders))]
        } else {
          sorted_folders <- sort(folders)
        }
        available_folders(sorted_folders)
      }
    })
  }

  # --- UNIFIED LAUNCH PARAMETER HANDSHAKE ---
  observe({
    query <- parseQueryString(session$clientData$url_search)

    opt_group <- getOption("biblioview.group", default = "")
    opt_key   <- getOption("biblioview.key",   default = "")
    opt_title <- getOption("biblioview.title", default = "")

    final_group <- if (!is.null(query$group)) query$group else opt_group
    final_key   <- if (!is.null(query$key))   query$key   else opt_key
    final_title <- if (!is.null(query$title)) query$title else opt_title

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
      tags$ul(class = "help-block-polite",
              style = "padding-left: 15px; margin-top: 5px; white-space: normal; word-wrap: break-word; line-height: 1.3;",
              tags$li("Providing a valid email address is polite open-access etiquette."),
              tags$li("Grants your requests access to the higher-priority OpenAlex Polite Pool.")
      ),
      br()
    )
  })

  # --- STEP 2 EXECUTION: BATCH CITATIONS ---
  observeEvent(input$citation_btn, {
    df <- current_dataset()
    req(df)

    ui_email <- if (!is.null(input$polite_email)) trimws(input$polite_email) else ""
    user_email <- if (ui_email != "") ui_email else Sys.getenv("POLITE_EMAIL")

    withProgress(message = 'Retrieving OpenAlex metrics...', value = 0.5, {
      df <- biblioview::fetch_citation_counts(df, email = user_email)
      current_dataset(df)
    })
  })

  # --- STEP 3 EXECUTION: MODAL INTERCEPT & ABSTRACT ENRICHMENT ---
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

  observeEvent(input$confirm_enrich_btn, {
    removeModal()
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

    export_title <- if (app_title() != "") app_title() else "export"
    clean_filename <- gsub("[^a-zA-Z0-9_-]", "_", export_title)

    formatted_df <- biblioview::format_hyperlinks(df)
    abstract_col_idx <- which(tolower(names(formatted_df)) == "abstract") - 1

    col_definitions <- list()
    if (length(abstract_col_idx) > 0 && !is.na(abstract_col_idx)) {
      col_definitions <- list(
        list(
          targets = abstract_col_idx,
          render = JS(
            "function(data, type, row) {",
            "  if (type === 'display' && data !== null && data.length > 90) {",
            "    var cleanText = data.replace(/\"/g, '&quot;').replace(/\\n/g, ' ');",
            "    return '<span title=\"' + cleanText + '\">' + data.substring(0, 90) + '...</span>';",
            "  }",
            "  return data;",
            "}"
          )
        )
      )
    }

    datatable(
      formatted_df,
      escape = FALSE,
      extensions = 'Buttons',
      options = list(
        dom = 'Blfrtip',
        buttons = list(
          list(extend = 'copy', title = NULL),
          list(extend = 'csv', filename = clean_filename, title = NULL),
          list(extend = 'excel', filename = clean_filename, title = NULL)
        ),
        pageLength = 15,
        lengthMenu = list(c(10, 15, 20, 50, 100, 200, -1), c('10', '15', '20', '50', '100', '200', 'All')),
        columnDefs = col_definitions
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
