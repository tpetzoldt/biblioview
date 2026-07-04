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

          # Step 1
          actionButton("fetch_btn", "1. Fetch Base Library", class = "btn-primary w-100"),
          br(), br(),

          # Step 2 (Controlled cleanly by conditional rendering)
          uiOutput("enrich_ui_container"),

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

      # Using click execution
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

      # Download is officially complete here
      current_dataset(raw)
    })
  })

  # 3. Dynamic UI component: Simply checks if data exists!
  output$enrich_ui_container <- renderUI({
    df <- current_dataset()

    # If download isn't complete, show nothing
    if (is.null(df)) {
      return(helpText("Fetch data to enable enrichment options."))
    }

    # If download is complete, enable Step 2 button instantly
    actionButton("enrich_btn", "2. Run Abstract Enrichment", class = "btn-warning w-100")
  })

  # 4. STEP 2: Enrichment Execution Loop
  observeEvent(input$enrich_btn, {
    df <- current_dataset()
    req(df)
    total_rows <- nrow(df)

    withProgress(message = 'Enriching library datasets...', value = 0, {
      # Pass the full table or process row by row depending on your package setup
      # If your function handles the entire data frame at once:
      df <- enrich_missing_abstracts(df)
      current_dataset(df)
    })
  })

  # 5. Output Table View
  output$bib_table <- renderDT({
    df <- current_dataset()
    req(df)

    # 5. Output Table View with Custom Length Menus
    output$bib_table <- renderDT({
      df <- current_dataset()
      req(df)

      datatable(
        biblioview::format_hyperlinks(df),
        escape = FALSE,
        extensions = 'Buttons',
        options = list(
          dom = 'Blfrtip',  # Added 'l' into the DOM layout string to display the length selection dropdown menu
          buttons = c('copy', 'csv', 'excel'),
          pageLength = 15,  # Default starting value
          lengthMenu = list(
            c(10, 15, 20, 50, 100, 200, -1), # Internal row values (-1 tells DT to show 'All' rows)
            c('10', '15', '20', '50', '100', '200', 'All') # Labels displayed in the UI dropdown selector
          )
        )
      )
    })
  })

  # 6. Sidebar Confirmation message string mapping
  output$status_text <- renderText({
    df <- current_dataset()
    if (is.null(df)) return("Ready to connect.")
    paste0("Loaded ", nrow(df), " entries successfully.")
  })
}

shinyApp(ui, server)
