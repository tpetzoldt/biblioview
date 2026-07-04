library(shiny)
library(shinydashboard)
library(biblioview)
library(dplyr)
library(DT)

ui <- dashboardPage(
  dashboardHeader(title = "Biblioview Portal"),
  dashboardSidebar(
    sidebarMenu(
      div(style = "padding: 15px;",
          textInput("group_id", "Zotero Group ID", value = ""),
          passwordInput("api_key", "API Key", value = ""),
          textInput("search_q", "Keyword Search (Optional)", value = ""),

          # Step 1: Base Fetch
          actionButton("fetch_btn", "1. Fetch Base Library", class = "btn-primary w-100"),
          br(), br(),

          # Step 2: Conditional Abstract Enrichment
          uiOutput("enrich_ui_container"),
          br(),

          # Step 3: Citation Metrics Enrichment
          uiOutput("citation_ui_container"),

          hr(),
          textOutput("status_text")
      )
    )
  ),
  dashboardBody(
    fluidRow(
      box(width = 12, title = "Searchable Reference Database", solidHeader = TRUE, status = "primary",
          DTOutput("bib_table")
      )
    )
  )
)

server <- function(input, output, session) {

  # Reactive variable that stays NULL until Zotero download is done
  current_dataset <- reactiveVal(NULL)

  # 1. Automated startup via URL parameters
  observe({
    query <- getQueryString()
    if (!is.null(query$group) && !is.null(query$key)) {
      updateTextInput(session, "group_id", value = query$group)
      updatePasswordInput(session, "api_key", value = query$key)
      if (!is.null(query$q)) updateTextInput(session, "search_q", value = query$q)

      click("fetch_btn")
    }
  })

  # 2. STEP 1: Fetch Base Data
  observeEvent(input$fetch_btn, {
    req(input$group_id, input$api_key)

    withProgress(message = 'Fetching from Zotero...', value = 0.5, {
      raw <- fetch_all_zotero_data(group_id = input$group_id, api_key = input$api_key)

      if (input$search_q != "") {
        raw <- raw |> filter(
          grepl(input$search_q, title, ignore.case = TRUE)
        )
      }

      current_dataset(raw)
    })
  })

  # 3. Dynamic UI for Step 2: Checks if data exists
  output$enrich_ui_container <- renderUI({
    df <- current_dataset()
    if (is.null(df)) return(NULL)

    actionButton("enrich_btn", "2. Run Abstract Enrichment", class = "btn-warning w-100")
  })

  # 4. Dynamic UI for Step 3: Unlocks alongside Step 2 when data is ready
  output$citation_ui_container <- renderUI({
    df <- current_dataset()
    if (is.null(df)) return(NULL)

    actionButton("citation_btn", "3. Fetch Citation Metrics", class = "btn-success w-100")
  })

  # 5. STEP 2 EXECUTION: Abstract Enrichment Loop
  observeEvent(input$enrich_btn, {
    df <- current_dataset()
    req(df)

    withProgress(message = 'Enriching missing library abstracts...', value = 0.5, {
      df <- enrich_missing_abstracts(df)
      current_dataset(df)
    })
  })

  # 6. STEP 3 EXECUTION: Fast Batch Citation Retrieval
  observeEvent(input$citation_btn, {
    df <- current_dataset()
    req(df)

    withProgress(message = 'Querying OpenAlex API for citation metrics...', value = 0.5, {
      # Calls your newly created package function using the POLITE_EMAIL env variable
      df <- biblioview::fetch_citation_counts(df)
      current_dataset(df)
    })
  })

  # 7. Output Table View with Custom Length Menus
  output$bib_table <- renderDT({
    df <- current_dataset()
    req(df)

    datatable(
      biblioview::format_hyperlinks(df),
      escape = FALSE, # Permits browser to parse HTML anchors securely
      extensions = 'Buttons',
      options = list(
        dom = 'Blfrtip',  # Includes 'l' to toggle page layout items dynamically
        buttons = c('copy', 'csv', 'excel'),
        pageLength = 15,  # Default starting row layout view
        lengthMenu = list(
          c(10, 15, 20, 50, 100, 200, -1),
          c('10', '15', '20', '50', '100', '200', 'All')
        )
      )
    )
  })

  # 8. Sidebar Confirmation text block mapping
  output$status_text <- renderText({
    df <- current_dataset()
    if (is.null(df)) return("Ready to connect.")

    status_msg <- paste0("Loaded ", nrow(df), " entries successfully.")

    # Check if citations were appended to give the user active context
    if ("citations" %in% names(df)) {
      valid_counts <- sum(!is.na(df$citations))
      status_msg <- paste0(status_msg, " (Citations mapped for ", valid_counts, " items).")
    }

    return(status_msg)
  })
}

shinyApp(ui, server)
