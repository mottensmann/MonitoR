library(bslib)
library(bsicons)
library(htmltools)
library(MonitoR)
library(shiny)
library(shinyFiles)
library(shinyjs)
library(tidyverse)
library(readxl)
library(writexl)

# UI ---------------------------------------------------------------------------

ui <- page_navbar(
  title = tags$span(bs_icon("binoculars-fill"), "BirdNET Analysis App"),
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  fillable = FALSE,
  shinyjs::useShinyjs(),
  # Add custom JavaScript for file link handling and Verification dropdown
  tags$head(
    tags$script(HTML("
      Shiny.addCustomMessageHandler('add_file_click_handlers', function(message) {
        $(document).on('click', '.file-link', function(e) {
          e.preventDefault();
          var filePath = $(this).data('file');
          Shiny.setInputValue('file_to_open', filePath, {priority: 'event'});
        });
      });
    "))
  ),

  # ── Tab 1: Configuration ───────────────────────────────────────────────────
  nav_panel(
    title = "Settings",
    icon  = bs_icon("gear"),

    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Data Settings"),
        shinyDirButton("path", "Select data folder", "Please select a folder"),
        div(class = "mt-2 p-2 bg-light rounded text-monospace text-break",
            style = "font-size: 0.9rem; min-height: 40px;",
            textOutput("path_display")),
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
        selectInput("model", "Model", choices = c("BirdNET v2.4", "Perch v2"))
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

  ## Tab 2: Processing ----
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
            title = "3 · Postprocessing",
            icon  = bs_icon("file-earmark-spreadsheet"),
            actionButton("run_format",  "Format results",
                         class = "btn-outline-success w-100 mb-1"),
            actionButton("run_filter",  "Filter by species (ornitho_de)",
                         class = "btn-outline-success w-100 mb-1"),
            actionButton("run_extract", "Extract detections",
                         class = "btn-outline-success w-100")
          ),

          accordion_panel(
            title = "4 · Archiving data",
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
        actionButton("save_results", "Save BirdNET.xlsx",
                     icon = icon("floppy-disk"), class = "btn-success w-100 mb-3"),
        uiOutput("taxon_ui"),
        input_switch("sync_filter",    "Filter table by taxon",           value = TRUE),
        input_switch("filter_quality", "Show 'Validate !' only",          value = FALSE),
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

  # Add JavaScript handler for file link clicks
  session$sendCustomMessage(type = "add_file_click_handlers", message = list())

  ## Log helpers ----
  log_rv <- reactiveVal(character(0))

  add_log <- function(msg, level = "INFO") {
    ts  <- format(Sys.time(), "%H:%M:%S")
    tag <- switch(level, INFO = "✔", WARN = "⚠", ERROR = "✖", "·")
    log_rv(c(log_rv(), paste0("[", ts, "] ", tag, " ", msg)))
  }

  output$log <- renderText(paste(log_rv(), collapse = "\n"))
  observeEvent(input$clear_log, log_rv(character(0)))

  ## Step 0: Pick folder ----

  roots <- c(Home = path.expand("~"), shinyFiles::getVolumes()())

  # Load last selected directory from cache file
  cache_file <- file.path(tempdir(), "monitor_last_dir.txt")
  last_dir <- tryCatch({
    if (file.exists(cache_file)) {
      dir <- readLines(cache_file, n = 1, warn = FALSE)
      if (dir.exists(dir)) dir else roots[1]
    } else {
      roots[1]
    }
  }, error = function(e) roots[1])

  selected_dir <- reactiveVal(last_dir)

  shinyDirChoose(input, "path", roots = roots, session = session)
  observeEvent(input$path, {
    if (is.null(input$path) || any(is.na(input$path))) return()
    new_dir <- parseDirPath(roots, input$path)
    selected_dir(new_dir)
    # Save to cache file
    tryCatch({
      writeLines(new_dir, cache_file)
    }, error = function(e) {
      warning("Could not save directory to cache: ", e$message)
    })
  })

  output$path_display <- renderText({
    path <- selected_dir()
    if (is.null(path) || !nzchar(path)) {
      "No folder selected"
    } else {
      path
    }
  })


  ## Step 1: Preprocessing ----
  observeEvent(input$run_rename, {
    req(selected_dir())
    add_log(paste("Renaming files in:", selected_dir()))
    tryCatch({
      MonitoR::strip_device_id(input_dir = selected_dir())
      add_log("Files renamed successfully")
    }, error = function(e) add_log(e$message, "ERROR"))
  })

  ## Step 2: BirdNET-Analyzer ----
  observeEvent(input$run_birdnet_r, {
    req(selected_dir())
    add_log("Scanning for wav files ...")
    tryCatch({
      wav_files <- list.files(
        selected_dir(), pattern = "\\.wav$",
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

      MonitoR::run_birdnet(
        wave_files          = wav_files,
        min_confidence      = input$min_conf,
        chunk_overlap_s     = input$overlap,
        sigmoid_sensitivity = input$sensitivity,
        model               = input$model,
        skip.existing.results = TRUE)
      add_log("Classification completed")
    }, error = function(e) add_log(e$message, "ERROR"))
  })

  ## Step 3.1.1: Format BirdNET results ----
  observeEvent(input$run_format, {
    req(selected_dir())
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
        Slist       = NA #input$slist
      )
      data <- lapply(
        selected_dir(), NocMigR2::BirdNET,
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
    req(selected_dir())
    add_log("Filter by species list ...")
    tryCatch({
      data <- MonitoR::birdNET_select(path = selected_dir())
      add_log(paste("BirdNET results filtered",  nrow(data), "records retained"))
    }, error = function(e) add_log(e$message, "ERROR"))
  })

  ## Step 3.2: Extract BirdNET results ----
  observeEvent(input$run_extract, {
    req(selected_dir())
    add_log("Extracting BirdNET results ...")
    tryCatch({
      lapply(
        selected_dir(), NocMigR2::BirdNET_extract,
        hyperlink = input$hyperlink,
        spectro   = input$spectro
      )
      add_log("Extraction completed")
    }, error = function(e) add_log(e$message, "ERROR"))
  })

  ## Step 4: Archive ----
  observeEvent(input$run_archive, {
    req(selected_dir(), input$path2archive, input$db)
    add_log("Archiving results...")
    tryCatch({
      NocMigR2::BirdNET_archive_am(
        BirdNET_results = file.path(selected_dir(), "BirdNET.xlsx"),
        path2archive    = input$path2archive,
        keep.false      = input$keep_false,
        db              = input$db,
        png             = input$png
      )
      add_log("Archive complete.")
    }, error = function(e) add_log(e$message, "ERROR"))
  })

  # Results: load BirdNET.xlsx ----
  results_data <- reactiveVal(NULL)

  observeEvent(input$load_results, {
    req(selected_dir())
    fp <- file.path(selected_dir(), "BirdNET.xlsx")
    if (!file.exists(fp)) {
      showNotification("BirdNET.xlsx not found. Run the workflow first.", type = "error")
      return()
    }
    df <- read_xlsx(fp)
    # Ensure editable columns exist
    if (!"Verification" %in% names(df)) df$Verification <- NA_character_
    if (!"Comment"      %in% names(df)) df$Comment      <- NA_character_
    if (!"Correction"   %in% names(df)) df$Correction   <- NA_character_
    # Ensure editable columns are always character (not logical when all-NA)
    df$Verification <- as.character(df$Verification)
    df$Comment      <- as.character(df$Comment)
    df$Correction   <- as.character(df$Correction)
    results_data(df)
  })

  # Save edited data back to xlsx
  observeEvent(input$save_results, {
    req(results_data(), selected_dir())
    fp <- file.path(selected_dir(), "BirdNET.xlsx")
    tryCatch({
      # Write Records sheet; preserve Meta sheet if present
      meta_fp <- file.path(selected_dir(), "BirdNET.xlsx")
      meta_df <- tryCatch(read_xlsx(meta_fp, sheet = "Meta"), error = function(e) NULL)
      sheets <- list(Records = results_data())
      if (!is.null(meta_df)) sheets$Meta <- meta_df
      writexl::write_xlsx(sheets, fp)
      showNotification("Saved successfully.", type = "message")
    }, error = function(e) {
      showNotification(paste("Save failed:", e$message), type = "error")
    })
  })

  # Store the edited data
  output$taxon_ui <- renderUI({
    req(results_data())
    taxa <- sort(unique(results_data()$Taxon))

    # Aktuellen Wert merken, falls bereits gesetzt
    current <- isolate(input$taxon)
    selected <- if (!is.null(current) && current %in% taxa) current else taxa[1]

    selectInput("taxon", "Focal taxon", choices = taxa,
                selected = selected, width = "100%")
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

  # Handle file link clicks to open in Audacity
  observeEvent(input$file_to_open, {
    file_path <- input$file_to_open
    if (is.null(file_path) || !nzchar(file_path)) return()

    # If path is relative, try to construct full path from selected directory
    if (!file.exists(file_path) && !is.null(selected_dir())) {
      full_path <- file.path(selected_dir(), file_path)
      if (file.exists(full_path)) {
        file_path <- full_path
      }
    }

    if (file.exists(file_path)) {
      tryCatch({
        # Open file with default application (Audacity if associated)
        shell.exec(file_path)
        showNotification(paste("Opening:", basename(file_path)), type = "message")
      }, error = function(e) {
        showNotification(paste("Error opening file:", e$message), type = "error")
      })
    } else {
      showNotification(paste("File not found:", file_path), type = "error")
    }
  })

  # Determine column indices that should NOT be editable (0-based for DT)
  # Editable: Verification, Comment, Correction — all others disabled
  editable_cols    <- c("Verification", "Comment", "Correction")
  display_col_order <- reactiveVal(NULL)   # tracks column order of the rendered table

  # Create a reactive for filtered data that updates when taxon OR sync_filter changes.
  # .rid stores the row index in results_data() so we can map back after DT filtering.
  filtered_data <- reactive({
    req(results_data(), input$taxon)
    df <- results_data()
    df$.rid <- seq_len(nrow(df))

    if (isTruthy(input$sync_filter) && !is.null(input$taxon)) {
      df <- df |> filter(Taxon == input$taxon)
    }
    if (isTruthy(input$filter_quality) && "Quality" %in% names(df)) {
      df <- df |> filter(Quality == "Validate !")
    }
    df
  })

  output$results_table <- DT::renderDataTable({
    df <- filtered_data()

    # Find file column for clickable links
    file_col <- NULL
    potential_cols <- c("Path", "path", "File", "file", "Filepath", "filepath", "FilePath")
    for (col in potential_cols) {
      if (col %in% names(df)) { file_col <- col; break }
    }
    if (is.null(file_col)) {
      for (col in names(df)) {
        if (is.character(df[[col]]) && any(grepl("[/\\\\]", df[[col]], na.rm = TRUE))) {
          file_col <- col; break
        }
      }
    }

    if (!is.null(file_col) && nrow(df) > 0) {
      df <- df |>
        mutate(
          !!file_col := paste0(
            '<a href="javascript:void(0);" class="file-link" data-file="',
            !!sym(file_col),
            '" style="color: #0066cc; text-decoration: underline; cursor: pointer;">',
            basename(!!sym(file_col)),
            '</a>'
          )
        )
    }

    # Column order: Taxon, File, rest (no T2)
    all_cols <- names(df)
    first_col <- all_cols[1]
    if (!is.null(file_col)) {
      remaining_cols <- setdiff(all_cols, c(first_col, file_col, "T2"))
      df <- df |> select(!!first_col, !!file_col, all_of(remaining_cols))
    } else {
      remaining_cols <- setdiff(all_cols, c(first_col, "T2"))
      df <- df |> select(!!first_col, all_of(remaining_cols))
    }

    # Cache display column order for use in the cell-edit observer
    display_col_order(names(df))

    # Determine which column indices (0-based) are NOT editable
    col_names   <- names(df)
    disable_idx <- which(!col_names %in% c(editable_cols, ".rid")) - 1L
    rid_idx     <- which(col_names == ".rid") - 1L  # 0-based, for JS and columnDefs

    DT::datatable(
      df,
      filter   = "top",
      escape   = FALSE,
      rownames = FALSE,
      editable = list(
        target  = "cell",
        disable = list(columns = disable_idx)
      ),
      options = list(
        pageLength = 15,
        scrollX    = TRUE,
        stateSave  = TRUE,
        # Hide the .rid helper column from users
        columnDefs = list(list(visible = FALSE, targets = rid_idx))
      ),
      callback = DT::JS(paste0("
        var ridCol   = ", rid_idx, ";
        var verifCol = -1;
        table.columns().every(function() {
          var header = $(this.header()).text().trim();
          if (header === 'Verification') { verifCol = this.index(); }
        });

        table.on('click', 'td', function(e) {
          if (verifCol < 0) return;
          var colIdx = table.cell(this).index().column;
          if (colIdx !== verifCol) return;

          e.stopImmediatePropagation();

          var cell   = table.cell(this);
          var curVal = cell.data() == null ? '' : String(cell.data());
          var td     = $(this);
          if (td.find('select.verif-dd').length) return;

          td.html(
            '<select class=\"verif-dd\" style=\"width:100%;padding:2px\">' +
            '<option value=\"\">—</option>' +
            '<option value=\"T\">T</option>' +
            '<option value=\"F\">F</option>' +
            '<option value=\"?\">?</option>' +
            '</select>'
          );
          var sel = td.find('select.verif-dd');
          sel.val(curVal).focus();

          sel.on('change', function() {
            var newVal = $(this).val();
            // Read .rid from this row's data — works regardless of DT filtering/pagination
            var rid    = table.cell(cell.index().row, ridCol).data();
            cell.data(newVal);
            Shiny.setInputValue(
              'verification_cell_edit',
              {row: rid, value: newVal, _nonce: Math.random()},
              {priority: 'event'}
            );
          });

          sel.on('blur', function() {
            setTimeout(function() {
              if (td.find('select.verif-dd').length) { td.html(curVal); }
            }, 200);
          });
        });
      "))
    )
  }, server = TRUE)

  # Observer for Verification (dropdown, custom JS event)
  # info$row is .rid — the direct row index in results_data() — set by JS
  observeEvent(input$verification_cell_edit, {
    info <- input$verification_cell_edit
    df   <- results_data()
    df[info$row, "Verification"] <- as.character(info$value)
    results_data(df)
  })

  # Observer for Comment / Correction (native DT inline editor)
  # info$row from DT is 1-based within filtered_data(); use .rid to map to results_data()
  observeEvent(input$results_table_cell_edit, {
    info     <- input$results_table_cell_edit
    df       <- results_data()
    col_name <- display_col_order()[info$col]

    if (is.null(col_name) || !col_name %in% c("Comment", "Correction")) return()

    master_row <- filtered_data()$.rid[info$row]
    df[master_row, col_name] <- as.character(info$value)
    results_data(df)
  })

  output$activity_plot <- renderPlot({
    req(results_data(), input$taxon)
    tryCatch(
      MonitoR::birdNET_graph(path = selected_dir(), taxon = isolate(input$taxon)),
      error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Error:", e$message), col = "red")
      }
    )
  })
}

shinyApp(ui, server)
