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

          # Step 1 Button
          actionButton("fetch_btn", "1. Fetch Base Library", class = "btn-primary w-100"),
          br(), br(),

          # Step 2 Button - Initially hidden or visually styled as secondary
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

  # A reactive container to hold our working data state over multiple steps
  current_dataset <- reactiveVal(NULL)

  # Helper function to convert raw text links to clickable HTML elements
  format_hyperlinks <- function(df) {
    if ("doi" %in% names(df)) {
      df <- df |>
        mutate(doi = ifelse(!is.na(doi) & doi != "",
                            paste0('<a href="https://doi.org/', doi, '" target="_blank">', doi, '</a>'),
                            doi))
    }
    if ("url" %in% names(df)) {
      df <- df |>
        mutate(url = ifelse(!is.na(url) & url != "",
                            paste0('<a href="', url, '" target="_blank">Link</a>'),
                            url))
    }
    return(df)
  }

  # 1. Listen for incoming URL configurations on startup
  observe({
    query <- getQueryString()
    if (!is.null(query$group) && !is.null(query$key)) {
      updateTextInput(session, "group_id", value = query$group)
      updatePasswordInput(session, "api_key", value = query$key)
      if (!is.null(query$q)) updateTextInput(session, "search_q", value = query$q)

      delay(10, click("fetch_btn"))
    }
  })

  # 2. STEP 1: Fast initial fetch (Zotero Only)
  observeEvent(input$fetch_btn, {
    req(input$group_id, input$api_key)

    withProgress(message = 'Fetching from Zotero...', value = 0.5, {
      raw <- fetch_all_zotero_data(group_id = input$group_id, api_key = input$api_key)

      if (input$search_q != "") {
        raw <- raw |> filter(
          grepl(input$search_q, title, ignore.case = TRUE) |
            grepl(input$search_q, abstractNote, ignore.case = TRUE)
        )
      }

      # Save the clean baseline data to our session state without enrichment
      current_dataset(raw)
    })
  })

  # 3. Dynamic UI component: Show Step 2 button only when data is loaded
  output$enrich_ui_container <- renderUI({
    df <- current_dataset()
    req(df)

    # Check if there are actually any missing abstracts to fix
    missing_count <- sum(is.na(df$abstract) | df$abstract == "")

    if (missing_count > 0) {
      actionButton("enrich_btn",
                   paste("2. Enrich Abstracts (", missing_count, " missing)"),
                   class = "btn-warning w-100")
    } else {
      helpText("All abstracts are fully populated!")
    }
  })

  # 4. STEP 2: Intentional Enrichment execution triggered by the user
  observeEvent(input$enrich_btn, {
    df <- current_dataset()
    req(df)

    # Identify row indicators matching our empty condition
    missing_indices <- which(is.na(df$abstract) | df$abstract == "")
    total_missing   <- length(missing_indices)

    withProgress(message = 'Querying External APIs...', value = 0, {

      for (i in seq_along(missing_indices)) {
        idx <- missing_indices[i]

        # Visual interface tracking callback
        incProgress(
          amount = 1 / total_missing,
          detail = paste0("Item [", i, "/", total_missing, "]: ", substring(df$title[idx], 1, 25), "...")
        )

        # Isolate and enrich just this single slice row
        enriched_row <- enrich_missing_abstracts(df[idx, ])

        # Splice the newly enriched metadata right back into the complete collection sheet
        df[idx, ] <- enriched_row

        # Push back into state iteratively so table updates smoothly if complex
        current_dataset(df)
      }
    })
  })

  # 5. Render database view with active hyperlinks
  output$bib_table <- renderDT({
    df <- current_dataset()
    req(df)

    datatable(
      format_hyperlinks(df), # Injected on fly to avoid overwriting character states in core records
      escape = FALSE,
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        pageLength = 15
      )
    )
  })

  # 6. Sidebar status indicator message string mapping
  output$status_text <- renderText({
    df <- current_dataset()
    if (is.null(df)) return("Enter credentials or provide URL parameters to begin.")

    missing_count <- sum(is.na(df$abstract) | df$abstract == "")
    paste0("Loaded ", nrow(df), " total entries. (", missing_count, " missing abstracts remaining).")
  })
}

shinyApp(ui, server)
