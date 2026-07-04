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

      # Shortened to 10ms for instant execution
      delay(10, click("fetch_btn"))
    }
  })

  # 2. Reactive calculation block with loading feedback
  fetched_data <- eventReactive(input$fetch_btn, {
    req(input$group_id, input$api_key)

    # Wrap in dynamic progress bar window
    withProgress(message = 'Connecting to Zotero...', value = 0.1, {

      incProgress(0.3, detail = "Downloading library items...")
      raw <- fetch_all_zotero_data(group_id = input$group_id, api_key = input$api_key)

      # Apply keyword filtering if provided
      if (input$search_q != "") {
        raw <- raw |> filter(
          grepl(input$search_q, title, ignore.case = TRUE) |
            grepl(input$search_q, abstractNote, ignore.case = TRUE)
        )
      }

      incProgress(0.4, detail = "Enriching missing abstracts...")
      data_processed <- enrich_missing_abstracts(raw)

      incProgress(0.2, detail = "Formatting hyperlinks...")

      # 3. Dynamic formatting step to convert strings into clickable tags
      # Adjust column names ('doi' and 'url') if your package uses different case structures
      if ("doi" %in% names(data_processed)) {
        data_processed <- data_processed |>
          mutate(doi = ifelse(!is.na(doi) & doi != "",
                              paste0('<a href="https://doi.org/', doi, '" target="_blank">', doi, '</a>'),
                              doi))
      }

      if ("url" %in% names(data_processed)) {
        data_processed <- data_processed |>
          mutate(url = ifelse(!is.na(url) & url != "",
                              paste0('<a href="', url, '" target="_blank">Link</a>'),
                              url))
      }

      data_processed
    })
  })

  # 4. Render the full table with HTML escaping turned OFF
  output$bib_table <- renderDT({
    req(fetched_data())
    datatable(
      fetched_data(),
      escape = FALSE, # CRITICAL: Allows the browser to render the clickable HTML links properly
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        pageLength = 15
      )
    )
  })

  # 5. Update the sidebar message status
  output$status_text <- renderText({
    if (input$fetch_btn == 0) return("Enter credentials or provide URL parameters to begin.")
    paste("Loaded", nrow(fetched_data()), "records successfully.")
  })
}

shinyApp(ui, server)
