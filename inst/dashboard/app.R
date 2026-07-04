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
        actionButton("fetch_btn", "Fetch Library", class = "btn-primary w-100", style = "margin-top: 10px;"),
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
  
  # 1. Listen for URL parameters on startup
  observe({
    query <- getQueryString()
    if (!is.null(query$group) && !is.null(query$key)) {
      updateTextInput(session, "group_id", value = query$group)
      updatePasswordInput(session, "api_key", value = query$key)
      if (!is.null(query$q)) updateTextInput(session, "search_q", value = query$q)
      
      # Small delay to let the inputs update before triggering the click
      delay(100, click("fetch_btn"))
    }
  })
  
  # 2. Reactive calculation block using your R package
  fetched_data <- eventReactive(input$fetch_btn, {
    req(input$group_id, input$api_key)
    
    # Run your package pipeline natively in R!
    raw <- fetch_all_zotero_data(group_id = input$group_id, api_key = input$api_key)
    
    # Apply keyword filtering if provided
    if (input$search_q != "") {
      raw <- raw |> filter(
        grepl(input$search_q, title, ignore.case = TRUE) | 
        grepl(input$search_q, abstractNote, ignore.case = TRUE)
      )
    }
    
    # Run your custom enrichment step
    enrich_missing_abstracts(raw)
  })
  
  # 3. Render the full table with export controls
  output$bib_table <- renderDT({
    req(fetched_data())
    datatable(
      fetched_data(),
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        pageLength = 15
      )
    )
  })
  
  # 4. Update the sidebar message status
  output$status_text <- renderText({
    if (input$fetch_btn == 0) return("Enter credentials or provide URL parameters to begin.")
    paste("Loaded", nrow(fetched_data()), "records successfully.")
  })
}

shinyApp(ui, server)
