### required libraries ----------------------------------------
pkgs <- c("bsicons", "bslib", "DT", "shiny", "shinyFiles",
          "shinyjs", "readxl", "thematic", "MonitoR")

MonitoR::.check_pkgs(pkgs = pkgs)

## UI ---------------------------------------------------------
ui <- page_navbar(
  title = tags$span(bsicons::bs_icon("binoculars-fill"), "BirdNET Analysis App"),
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  fillable = FALSE,
  shinyjs::useShinyjs(),

  ### Tab 1: Settings -----------------------------------------
  nav_panel(
    title = "Settings",
    icon  = bsicons::bs_icon("gear"),

    card(
      card_header("Database"),
      shinyFilesButton("db", "Select database (db.xlsx)", "Please select the database file",
                       multiple = FALSE, class = "w-100"),
      div(class = "mt-2 p-2 bg-light rounded text-monospace text-break",
          style = "font-size: 0.9rem; min-height: 40px;",
          textOutput("db_display"))
    )
  ),

  ### Tab 2: Results ------------------------------------------
  nav_panel(
    title = "Results",
    icon  = bsicons::bs_icon("database"),

    layout_columns(
      col_widths = c(3, 9),

      card(
        card_header("Controls"),
        actionButton("load_results", "Load results",
                     icon = icon("folder-open"), class = "btn-primary w-100 mb-3"),
        uiOutput("taxon_ui"),
        input_switch("sync_filter", "Filter table by taxon", value = TRUE),
        hr(),
        uiOutput("summary_boxes")
      ),

      navset_card_underline(
        full_screen = TRUE,
        nav_panel(
          "Detections",
          DT::dataTableOutput("results_table")
        ),
        nav_panel(
          "Heatmap",
          plotOutput("heatmap", height = "420px")
        )
      )
    )
  )
)


## Server -----------------------------------------------------
server <- function(input, output, session) {

  thematic::thematic_shiny()

  roots      <- c(Home = path.expand("~"), shinyFiles::getVolumes()())
  cache_file <- path.expand("~/monitor_db.txt")

  read_cache <- function() {
    defaults <- list(db = "")
    tryCatch({
      if (!file.exists(cache_file)) return(defaults)
      lines <- readLines(cache_file, warn = FALSE)
      pairs <- strsplit(lines, "=", fixed = TRUE)
      for (p in pairs) if (length(p) == 2) defaults[[trimws(p[1])]] <- trimws(p[2])
      defaults
    }, error = function(e) defaults)
  }

  write_cache <- function(key, value) {
    tryCatch({
      current        <- read_cache()
      current[[key]] <- value
      writeLines(paste0(names(current), "=", unlist(current)), cache_file)
    }, error = function(e) warning("Could not write cache: ", e$message))
  }

  ### db.xlsx selection ---------------------------------------
  cache       <- read_cache()
  last_db     <- if (nzchar(cache$db) && file.exists(cache$db)) cache$db else ""
  selected_db <- reactiveVal(last_db)

  shinyFileChoose(input, "db", roots = roots, session = session, filetypes = "xlsx")
  observeEvent(input$db, {
    if (is.null(input$db) || any(is.na(input$db))) return()
    nf <- parseFilePaths(roots, input$db)
    if (nrow(nf) == 0) return()
    fp <- as.character(nf$datapath)
    selected_db(fp)
    write_cache("db", fp)
  })

  output$db_display <- renderText({
    fp <- selected_db()
    if (is.null(fp) || !nzchar(fp)) "No file selected" else fp
  })

  ### Results -------------------------------------------------
  results_data <- reactiveVal(NULL)

  observeEvent(input$load_results, {
    fp <- selected_db()
    if (!nzchar(fp) || !file.exists(fp)) {
      showNotification("No database file selected or file not found.", type = "error")
      return()
    }
    tryCatch({
      df <- readxl::read_xlsx(fp)
      results_data(df)
      showNotification("Loaded successfully.", type = "message")
    }, error = function(e) {
      showNotification(paste("Load failed:", e$message), type = "error")
    })
  })

  output$taxon_ui <- renderUI({
    req(results_data())
    taxa    <- sort(unique(results_data()$Taxon))
    current <- isolate(input$taxon)
    selected <- if (!is.null(current) && current %in% taxa) current else taxa[1]
    selectInput("taxon", "Focal taxon", choices = taxa, selected = selected, width = "100%")
  })

  output$summary_boxes <- renderUI({
    req(results_data())
    df <- results_data()
    tagList(
      value_box("Detections", nrow(df),                theme = "primary",
                showcase = bsicons::bs_icon("soundwave")),
      value_box("Species",    dplyr::n_distinct(df$Taxon), theme = "success",
                showcase = bsicons::bs_icon("feather"))
    )
  })

  filtered_data <- reactive({
    req(results_data())
    df <- results_data()
    if (isTruthy(input$sync_filter) && !is.null(input$taxon))
      df <- dplyr::filter(df, Taxon == input$taxon)
    df
  })

  output$results_table <- DT::renderDataTable({
    DT::datatable(
      filtered_data(),
      filter   = "top",
      rownames = FALSE,
      options  = list(pageLength = 15, scrollX = TRUE, stateSave = TRUE)
    )
  }, server = TRUE)

  output$heatmap <- renderPlot({
    req(selected_db(), input$taxon)
      #MonitoR::birdNET_heatmap(db = isolate(input$db), taxon = isolate(input$taxon))
      MonitoR::birdNET_graph(path = dirname(selected_db()), taxon = isolate(input$taxon), model = input$model)

  })
}

shinyApp(ui, server)
