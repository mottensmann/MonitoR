library(bslib)
library(bsicons)
library(MonitoR)
library(shiny)
library(shinyFiles)
library(tidyverse)
library(readxl)


# UI ---------------------------------------------------------------------------

ui <- page_navbar(
  title = tags$span(bs_icon("binoculars-fill"), "BirdNET Analysis App"),
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  fillable = FALSE,

  # ── Tab 1: Configuration ───────────────────────────────────────────────────
  nav_panel(
    title = "Meta-data",
    icon  = bs_icon("gear"),

    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Data Settings"),
        textInput("path", "Path to data files",
                  value = "D:/BirdNET/",
                  width = "100%"),
        input_switch("am_config",  "Load AudioMoth config (CONFIG.txt)", value = TRUE),
        input_switch("recursive",  "Recursive directory search",          value = TRUE),
        input_switch("hyperlink",  "Create hyperlinks", value = TRUE),
        input_switch("spectro",    "Create spectrograms", value = FALSE)
      ),

      card(
        card_header("BirdNET Meta Data"),
        layout_columns(
          col_widths = c(6, 6),
          textInput("location", "Location name", value = "None"),
          selectInput("device", "Recorder", choices = c("AudioMoth", "SongMeter"))
        ),
        layout_columns(
          col_widths = c(6, 6),
          numericInput("lat", "Latitude",  value = NA),
          numericInput("lon", "Longitude", value = NA)
        ),
        textInput("micro", "External Microphone", value = ""),
        layout_columns(
          col_widths = c(4, 4, 4),
          numericInput("min_conf",    "Min. Confidence", value = 0.7,  min = 0,   max = 1,   step = 0.05),
          numericInput("overlap",     "Overlap",         value = 0,    min = 0,   max = 1,   step = 0.1),
          numericInput("sensitivity", "Sensitivity",     value = 1.25, min = 0.5, max = 1.5, step = 0.05)
        ),
        selectInput("slist", "Model", choices = c("BirdNET_V2.4", "Perch v2"))
      )
    ),

    card(
      card_header("Archive Settings"),
      layout_columns(
        col_widths = c(5, 5, 1, 1),
        textInput("path2archive", "Archive path",           value = "D:/BirdNET/test/Records/", width = "100%"),
        textInput("db",           "Database path (.xlsx)",  value = "D:/BirdNET/test/db.xlsx",  width = "100%"),
        div(class = "mt-2", input_switch("keep_false", "Keep false positives", value = FALSE)),
        div(class = "mt-2", input_switch("png",        "Export PNG",           value = FALSE))
      )
    )
  ),

  # ── Tab 2: Workflow ────────────────────────────────────────────────────────
  nav_panel(
    title = "Processing",
    icon  = bs_icon("play-btn"),

    layout_columns(
      col_widths = c(3, 9),

      card(
        card_header("Run Steps"),
        p(class = "text-muted small", "Execute each step in sequence."),
        accordion(
          open = TRUE,

          accordion_panel(
            title = "1 · Preprocessing",
            icon  = bs_icon("folder2-open"),
            actionButton("run_rename", "Rename files to datetime",
                         class = "btn-outline-success w-100 mb-1")
          ),

          accordion_panel(
            title = "2 · AI Classification",
            icon  = bs_icon("cpu"),
            #hr(class = "my-2"),
            actionButton("run_birdnet_r", "Run BirdNET (birdnetR)",
                         class = "btn-outline-success w-100")
          ),

          accordion_panel(
            title = "3 · Postprocessing BirdNET output",
            icon  = bs_icon("file-earmark-spreadsheet"),
            actionButton("run_format",  "Format results",
                         class = "btn-outline-success w-100 mb-1"),
            actionButton("run_filter",  "Filter by species (ornitho_de)",
                         class = "btn-outline-success w-100 mb-1"),
            actionButton("run_extract", "Extract detections",
                         class = "btn-outline-success w-100")
          ),

          accordion_panel(
            title = "4 · Archiving",
            icon  = bs_icon("archive"),
            actionButton("run_archive", "Archive results",
                         class = "btn-outline-danger w-100")
          )
        )
      ),

      card(
        full_screen = TRUE,
        card_header(
          class = "d-flex justify-content-between align-items-center",
          "Log",
          actionButton("clear_log", "Clear",
                       class = "btn-sm btn-outline-secondary")
        ),
        verbatimTextOutput("log", placeholder = TRUE)
      )
    )
  ),

  # ── Tab 3: Results ─────────────────────────────────────────────────────────
  nav_panel(
    title = "Results",
    icon  = bs_icon("database"),

    layout_columns(
      col_widths = c(3, 9),

      card(
        card_header("Controls"),
        actionButton("load_results", "Load BirdNET.xlsx",
                     icon = icon("folder-open"), class = "btn-primary w-100 mb-3"),
        uiOutput("taxon_ui"),
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
          "Activity plot",
          plotOutput("activity_plot", height = "420px")
        )
      )
    )
  )
)


# Server -----------------------------------------------------------------------

server <- function(input, output, session) {
  thematic::thematic_shiny()

  # Set default root and default folder
  roots <- c(default = "D:/BirdNET/")

  shinyFiles::shinyDirChoose(input, "path", roots = roots)

  ## Log helpers ----
  log_rv <- reactiveVal(character(0))

  add_log <- function(msg, level = "INFO") {
    ts  <- format(Sys.time(), "%H:%M:%S")
    tag <- switch(level, INFO = "✔", WARN = "⚠", ERROR = "✖", "·")
    log_rv(c(log_rv(), paste0("[", ts, "] ", tag, " ", msg)))
  }

  output$log <- renderText(paste(log_rv(), collapse = "\n"))
  observeEvent(input$clear_log, log_rv(character(0)))

  # observeEvent(input$pick_folder, {
  #   folder <- choose.dir(default = "D:/BirdNET/test/")
  #   if (!is.na(folder)) {
  #     updateTextInput(session, "path", value = folder)
  #   }
  # })

  ## Step 1: Preprocessing ----
  observeEvent(input$run_rename, {
    req(input$path)
    add_log(paste("Renaming files in:", input$path))
    tryCatch({
      MonitoR::strip_device_id(input_dir = input$path)
      add_log("Files renamed successfully")
    }, error = function(e) add_log(e$message, "ERROR"))
  })

  ## Step 2: BirdNET-Analyzer ----
  observeEvent(input$run_birdnet_r, {
    req(input$path)
    add_log("Scanning for wav files ...")
    tryCatch({
      wav_files <- list.files(
        input$path, pattern = "\\.wav$",
        full.names = TRUE, recursive = input$recursive, ignore.case = TRUE
      )
      ## ignore subfolder 'extracted' if present
      wav_files <- wav_files[!stringr::str_detect(wav_files, "extracted")]
      if (length(wav_files) == 0) {
        add_log("No .wav files found in the specified path.", "WARN")
        return()
      }
      add_log(paste("Found", length(wav_files), "file(s)"))
      add_log(paste("Starting birdnetR ..."))

      birdNET_process_batch(
        wave_files          = wav_files,
        min_confidence      = input$min_conf,
        chunk_overlap_s     = input$overlap,
        sigmoid_sensitivity = input$sensitivity
      )
      add_log("Classification completed")
    }, error = function(e) add_log(e$message, "ERROR"))
  })

  ## Step 3.1.1: Format BirdNET results ----
  observeEvent(input$run_format, {
    req(input$path)
    add_log("Reformatting BirdNET results ...")
    tryCatch({
      meta <- NocMigR2::BirdNET_meta(
        Location    = if (nzchar(input$location)) input$location else NA,
        Lat         = input$lat,
        Lon         = input$lon,
        Device      = input$device,
        Micro       = if (nzchar(input$micro)) input$micro else NA,
        Min_conf    = input$min_conf,
        Overlap     = input$overlap,
        Sensitivity = input$sensitivity,
        Slist       = input$slist
      )
      data <- lapply(
        input$path, NocMigR2::BirdNET,
        am_config = input$am_config,
        recursive = input$recursive,
        meta      = meta
      )
      n <- nrow(data[[1]][['Records']])
      add_log(paste("BirdNET results formatted", n, "records found"))
    }, error = function(e) add_log(e$message, "ERROR"))
  })

  ## Step 3.1.2: Filter BirdNET results ----
  observeEvent(input$run_filter, {
    req(input$path)
    add_log("Filter by species list ...")
    tryCatch({
      data <- MonitoR::birdNET_select(path = input$path)
      add_log(paste("BirdNET results filtered",  nrow(data), "records retained"))
    }, error = function(e) add_log(e$message, "ERROR"))
  })

  ## Step 3.2: Extract BirdNET results ----
  observeEvent(input$run_extract, {
    req(input$path)
    add_log("Extracting BirdNET results ...")
    tryCatch({
      lapply(
        input$path, NocMigR2::BirdNET_extract,
        hyperlink = input$hyperlink,
        spectro   = input$spectro
      )
      add_log("Extraction completed")
    }, error = function(e) add_log(e$message, "ERROR"))
  })

  ## Step 4: Archive ----
  observeEvent(input$run_archive, {
    req(input$path, input$path2archive, input$db)
    add_log("Archiving results...")
    tryCatch({
      NocMigR2::BirdNET_archive_am(
        BirdNET_results = file.path(input$path, "BirdNET.xlsx"),
        path2archive    = input$path2archive,
        keep.false      = input$keep_false,
        db              = input$db,
        png             = input$png
      )
      add_log("Archive complete.")
    }, error = function(e) add_log(e$message, "ERROR"))
  })

  # Results: load BirdNET.xlsx ----
  results_data <- eventReactive(input$load_results, {
    req(input$path)
    fp <- file.path(input$path, "BirdNET.xlsx")
    if (!file.exists(fp)) {
      showNotification("BirdNET.xlsx not found. Run the workflow first.", type = "error")
      return(NULL)
    }
    read_xlsx(fp)
  })

  output$taxon_ui <- renderUI({
    req(results_data())
    taxa <- sort(unique(results_data()$Taxon))
    selectInput("taxon", "Focal taxon", choices = taxa, width = "100%")
  })

  output$summary_boxes <- renderUI({
    req(results_data())
    df   <- results_data()
    n    <- nrow(df)
    ntax <- n_distinct(df$Taxon)
    tagList(
      value_box("Detections", n,    theme = "primary", showcase = bs_icon("soundwave")),
      value_box("Species",    ntax, theme = "success", showcase = bs_icon("feather"))
    )
  })

  output$results_table <- DT::renderDataTable({
    req(results_data())
    DT::datatable(
      results_data(),
      filter  = "top",
      options = list(pageLength = 15, scrollX = TRUE)
    )
  })

  output$activity_plot <- renderPlot({
    req(results_data(), input$taxon)
    tryCatch(
      birdNET_graph(path = input$path, taxon = input$taxon),
      error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Error:", e$message), col = "red")
      }
    )
  })
}

shinyApp(ui, server)
