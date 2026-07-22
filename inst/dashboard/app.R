library(shinydashboard)
library(biblioview)
library(dplyr)
library(DT)
library(stringr)

ui <- dashboardPage(
  title = "Biblioview Portal", # <-- Sets the default HTML metadata title for the browser tab
  dashboardHeader(
    title = uiOutput("dynamic_title"), # <-- Dynamically renders the project name upper left
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

          # Static selectizeInput (remains permanently bound in DOM so backspace/clear works cleanly)
          selectizeInput("selected_folders", "Folders (Leave blank for all)",
                         choices = NULL, multiple = TRUE,
                         options = list(placeholder = 'Scan folders first...')),
          br(),

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
              # Dynamically reads the active Version field from the DESCRIPTION file
              span(paste0("v", utils::packageVersion("biblioview"), " | ", format(Sys.Date(), "%Y-%m-%d"))),
              tags$a(href = "https://github.com/tpetzoldt/biblioview", target = "_blank",
                     style = "color: #3d8d8d; text-decoration: underline;",
                     tags$i(class = "fa fa-github", style = "color: #3d8d8d; text-decoration: underline;"), "https://github.com/tpetzoldt")
          )
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* --- Existing Table and Helper Text Styles --- */
        .dataTable tbody td {
          vertical-align: top !important;
        }
        /* Style adjustments for the small helper text under the email input */
        .help-block-polite {
          font-size: 0.85em;
          color: #b8c7ce;
          margin-top: 5px;
          line-height: 1.3;
        }

        /* --- Sidebar Width Overrides (Expanded State) --- */
        .main-sidebar {
          width: 300px !important;
        }
        .content-wrapper, .main-footer, .right-side {
          margin-left: 300px !important;
        }

        /* --- Sidebar Collapsed State Fixes (Burger Menu Clicked) --- */
        .sidebar-collapse .main-sidebar {
          transform: translate(-300px, 0) !important;
        }
        .sidebar-collapse .content-wrapper,
        .sidebar-collapse .main-footer,
        .sidebar-collapse .right-side,
        .sidebar-collapse .main-header .navbar {
          margin-left: 0px !important;
        }

        /* Smooth out layout shifting transitions */
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

  available_folders  <- reactiveVal(NULL)
  current_dataset    <- reactiveVal(NULL)
  app_title          <- reactiveVal("Biblioview Portal")
  url_processed      <- reactiveVal(FALSE)

  output$dynamic_title <- renderUI({
    tags$span(app_title())
  })

  # Helper function to extract Note: lines from Extra
  process_extra_notes <- function(df) {
    if ("extra" %in% names(df)) {
      df <- df |>
        mutate(Note = stringr::str_match(extra, "(?im)^note:\\s*(.*)$")[, 2]) |>
        select(!extra)
    }
    return(df)
  }

  # --- CENTRALIZED SCAN FUNCTION ---
  # run_folder_scan <- function(target_group, target_key) {
  #   withProgress(message = 'Scanning group folders...', value = 0.5, {
  #     folders <- biblioview::fetch_zotero_collections(target_group, target_key)
  #
  #     if (length(folders) == 0) {
  #       showNotification("No sub-folders found or invalid credentials. Showing root library by default.", type = "warning")
  #       available_folders(c("All Folders (Root)" = "ROOT"))
  #     } else {
  #       # --- ALPHABETICAL SORTING ENGINE ---
  #       # Sorts the named vector by its names (the human-readable titles)
  #       if (!is.null(names(folders))) {
  #         sorted_folders <- folders[order(names(folders))]
  #       } else {
  #         # Fallback if it's just a regular unnamed vector of strings
  #         sorted_folders <- sort(folders)
  #       }
  #       available_folders(sorted_folders)
  #     }
  #   })
  # }

  # ----------------------------------------------------------------------------

  # --- HIERARCHICAL SIDEBAR SCAN INTERFACE ---
  run_folder_scan <- function(target_group, target_key, target_folder_names = NULL) {
    matched_keys <- NULL

    withProgress(message = 'Mapping folder hierarchy...', value = 0.5, {

      # 1. Fetch the raw metadata definitions directly from the API to read the parent links
      coll_url <- paste0("https://api.zotero.org/groups/", target_group, "/collections")
      coll_res <- httr::GET(coll_url, httr::add_headers("Zotero-API-Key" = target_key))

      if (httr::status_code(coll_res) != 200) {
        showNotification("Could not read hierarchy metadata. Falling back to flat view.", type = "warning")
        folders <- biblioview::fetch_zotero_collections(target_group, target_key)
        sorted_f <- folders[order(names(folders))]
        available_folders(sorted_f)
        updateSelectizeInput(session, "selected_folders", choices = sorted_f, selected = character(0))
        return(NULL)
      }

      raw_json <- jsonlite::fromJSON(httr::content(coll_res, "text", encoding = "UTF-8"), simplifyVector = FALSE)

      if (length(raw_json) == 0) {
        root_f <- c("All Folders (Root)" = "ROOT")
        available_folders(root_f)
        updateSelectizeInput(session, "selected_folders", choices = root_f, selected = character(0))
        return(NULL)
      }

      # 2. Build a reliable local relationship table
      df_tree <- data.frame(
        key = sapply(raw_json, function(x) x$key),
        name = sapply(raw_json, function(x) x$data$name),
        parent = sapply(raw_json, function(x) {
          p <- x$data$parentCollection
          if (is.null(p) || is.logical(p) || p == "" || p == "FALSE") NA_character_ else p
        }),
        stringsAsFactors = FALSE
      )

      # Resolve folder keys from folder names if requested
      if (!is.null(target_folder_names) && target_folder_names != "") {
        req_folders <- trimws(unlist(strsplit(target_folder_names, ",")))
        matched_keys <- df_tree$key[tolower(df_tree$name) %in% tolower(req_folders)]

        # Fallback: Check if user passed direct collection keys
        if (length(matched_keys) == 0) {
          matched_keys <- df_tree$key[df_tree$key %in% req_folders]
        }
      }

      hierarchical_choices <- c()

      # 3. Recursive Engine: Alphabetize top levels, then recursively print sorted sub-folders
      build_branch <- function(current_parent = NA, depth = 0) {
        # Isolate items at this structural depth level
        if (is.na(current_parent)) {
          level_nodes <- df_tree[is.na(df_tree$parent), ]
        } else {
          level_nodes <- df_tree[!is.na(df_tree$parent) & df_tree$parent == current_parent, ]
        }

        if (nrow(level_nodes) == 0) return()

        # Sort the current level branches alphabetically by name
        level_nodes <- level_nodes[order(level_nodes$name), ]

        for (i in seq_len(nrow(level_nodes))) {
          this_key  <- level_nodes$key[i]
          this_name <- level_nodes$name[i]

          # Create a prefix using regular text dashes (e.g., "- Subfolder" or "-- Grandchild")
          prefix <- if (depth > 0) {
            # Creates a modern "└─ " or "  └─ " nesting marker
            indent <- paste0(rep("\u00a0\u00a0", depth - 1), collapse = "")
            paste0(indent, "\u2514\u2500\u00a0")
          } else {
            ""
          }

          # Alternative: Open circle prefix
          # prefix <- if (depth > 0) {
          #   indent <- paste0(rep("\u00a0\u00a0", depth), collapse = "")
          #   paste0(indent, "\u25e6\u00a0")
          # } else {
          #   ""
          # }


          display_label <- paste0(prefix, this_name)

          # Store the item (UI display text = hidden API hash key value)
          hierarchical_choices <<- c(hierarchical_choices, stats::setNames(this_key, display_label))

          # Instantly descend to process any children belonging beneath this folder before moving to the next sibling
          build_branch(current_parent = this_key, depth = depth + 1)
        }
      }

      # Run the tree assembler starting from the root folders
      build_branch(current_parent = NA, depth = 0)

      choices_to_use <- if (length(hierarchical_choices) == 0) {
        folders <- biblioview::fetch_zotero_collections(target_group, target_key)
        folders[order(names(folders))]
      } else {
        hierarchical_choices
      }

      available_folders(choices_to_use)

      # Update choices in the persistent input box cleanly
      selected_val <- if (!is.null(matched_keys)) matched_keys else character(0)
      updateSelectizeInput(session, "selected_folders",
                           choices = choices_to_use,
                           selected = selected_val,
                           options = list(placeholder = 'Select one or more folders'))
    })

    return(matched_keys)
  }

  # ----------------------------------------------------------------------------

  # --- UNIFIED LAUNCH PARAMETER HANDSHAKE ---
  observe({
    req(!url_processed()) # Executes strictly once at initial session startup

    # 1. Capture URL query strings (Primary for Shiny Server deployments)
    query <- parseQueryString(session$clientData$url_search)

    # 2. Capture R global options (Primary for local launch_dashboard() function)
    opt_group  <- getOption("biblioview.group",  default = "")
    opt_key    <- getOption("biblioview.key",    default = "")
    opt_title  <- getOption("biblioview.title",  default = "")
    opt_folder <- getOption("biblioview.folder", default = "")

    # 3. Resolve prioritization: URL parameters take precedence over function arguments
    final_group  <- if (!is.null(query$group))  query$group  else opt_group
    final_key    <- if (!is.null(query$key))    query$key    else opt_key
    final_title  <- if (!is.null(query$title))  query$title  else opt_title
    final_folder <- if (!is.null(query$folder)) query$folder else opt_folder

    if (final_group != "" && final_key != "") {
      session$onFlushed(function() {
        updateTextInput(session, "group_id", value = final_group)
        updateTextInput(session, "api_key", value = final_key)
      })

      if (final_title != "") {
        app_title(final_title)
      }

      # Run scan and collect resolved folder keys
      target_keys <- run_folder_scan(final_group, final_key, target_folder_names = final_folder)

      # ONLY execute auto-fetch if a folder argument was explicitly provided
      if (!is.null(final_folder) && final_folder != "") {
        withProgress(message = 'Retrieving reference entries...', value = 0.5, {
          raw <- fetch_all_zotero_data(
            group_id      = final_group,
            api_key       = final_key,
            collection_id = target_keys
          )
          raw <- process_extra_notes(raw)
          current_dataset(raw)
        })
      }

      url_processed(TRUE)
    }
  })

  # --- STEP 0: MANUAL SCAN BUTTON ---
  observeEvent(input$scan_btn, {
    req(input$group_id, input$api_key)
    run_folder_scan(input$group_id, input$api_key)
  })

  # --- STEP 1: FETCH LIBRARY ---
  output$fetch_ui_container <- renderUI({
    if (is.null(available_folders())) return(NULL)
    actionButton("fetch_btn", "1. Fetch Selected Folder(s)", class = "btn-warning w-100")
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

      raw <- process_extra_notes(raw)
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

  # --- STEP 2 EXECUTION: BATCH CITATIONS ---
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

    export_title <- if (app_title() != "") app_title() else "export"
    clean_filename <- gsub("[^a-zA-Z0-9_-]", "_", export_title)

    # 1. Format the data first so we use the exact structure sent to DT
    formatted_df <- biblioview::format_hyperlinks(df)

    # 2. Find column indices safely on formatted data
    abstract_col_idx <- which(tolower(names(formatted_df)) == "abstract")
    note_col_idx     <- which(tolower(names(formatted_df)) %in% c("note", "extra_note"))

    # 3. Apply standard substring clipping if column indices exist
    col_definitions <- list()

    if (length(abstract_col_idx) > 0 && !is.na(abstract_col_idx)) {
      col_definitions[[length(col_definitions) + 1]] <- list(
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
    }

    if (length(note_col_idx) > 0 && !is.na(note_col_idx)) {
      col_definitions[[length(col_definitions) + 1]] <- list(
        targets = note_col_idx,
        render = JS(
          "function(data, type, row) {",
          "  if (type === 'display' && data !== null && data.length > 60) {",
          "    var cleanText = data.replace(/\"/g, '&quot;').replace(/\\n/g, ' ');",
          "    return '<span title=\"' + cleanText + '\">' + data.substring(0, 60) + '...</span>';",
          "  }",
          "  return data;",
          "}"
        )
      )
    }

    datatable(
      formatted_df,
      escape = FALSE,
      extensions = 'Buttons',
      filter = "top",
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
