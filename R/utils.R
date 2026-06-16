#' Rename file names to YYYYMMDD_HHMMSS format
#' @description
#' Retrieve timestamps from file meta data and rename accordingly. If output_dir is specified files are copied, otherwise renamed.
#'
#' @param input_dir input folder
#' @param output_dir output folder
#' @param recursive boolean
#' @param pattern file extension
#' @param prefix string
#' @param dtime 'mtime' or 'ctime'
#' @param tz time zone
#' @export
#'
rename_photo2datetime <- function(input_dir,
                                  output_dir = NULL,
                                  recursive = T,
                                  pattern = c("JPG", "JPEG", "PNG"),
                                  prefix = NULL,
                                  dtime = c("ctime", "mtime"),
                                  tz = '') {
  pattern <- match.arg(pattern)
  dtime <- match.arg(dtime)

  ## list files
  if (!is.null(pattern)) {
    files <- list.files(input_dir, full.names = T, recursive = recursive, pattern = pattern)
  } else {
    files <- list.files(input_dir, full.names = T, recursive = recursive)
  }

  ## Retrieve date & time
  cat('retrieve timestamps ... \n')
  if (pattern %in% c("JPG", "JPEG", "PNG")) {
    timestamp <- exifr::read_exif(files, tags = 'DateTimeOriginal')[['DateTimeOriginal']]
    timestamp <-  as.POSIXct(timestamp, format = "%Y:%m:%d %H:%M:%S", tz = tz)
  } else {
    timestamp <- as.vector(sapply(files, function(x) file.info(x)[[dtime]]))
    timestamp <- as.POSIXct(timestamp, origin = "1970-01-01", tz = tz)
  }
  timestamp <- format(timestamp, "%Y%m%d_%H%M%S")

  ## Create file name from timestamp
  if (any(duplicated(timestamp))) timestamp <- paste0(timestamp, '_', 1:length(timestamp))
  if (!is.null(prefix)) timestamp <- paste0(prefix, '_', timestamp)
  timestamp <- paste0(timestamp, '.', tools::file_ext(files))

  ## rename file
  cat('rename files ... \n')
  if (is.null(output_dir)) {
    out <- pbapply::pblapply(1:length(files), function(x) {
      file.rename(from = file.path(dirname(files[x]), basename(files[x])),
                  to = file.path(dirname(files[x]), timestamp[x]))
    })
    ## copy
  } else if (!is.null(output_dir)) {
    if (!dir.exists(output_dir)) dir.create(output_dir, showWarnings = FALSE)
    out <- pbapply::pblapply(1:length(files), function(x) {
      file.copy(from = file.path(dirname(files[x]), basename(files[x])),
                to = file.path(output_dir, timestamp[x]))
    })
  }
}

#' Strip device used as prefix id from file names otherwise representing just a time-stamp.
#' @description
#' Retrieves timestamps from file names, expecting format YYYMMDD_HHMMSS as the sub-fix.
#'
#' @details
#' Use with caution as the functions simply retrieves the last 14 characters of the file name. If unsure set \code{.simulate = TRUE}. Folders named 'extracted' are skipped!
#'
#'
#' @param input_dir input folder
#' @param recursive boolean
#' @param pattern file pattern
#' @param ignore.case boolean
#' @param .simulate boolean
#' @export
#'
strip_device_id <- function(input_dir, pattern = ".WAV", recursive = T, ignore.case = T, .simulate = F) {

  ## list files based on pattern
  wave_files <- list.files(input_dir, full.names = T, recursive = T, pattern = pattern, ignore.case = ignore.case)

  # Filter out any files from the folder "extracted"
  wave_files <- wave_files[!grepl("extracted/", wave_files)]

  ## prompt a warning if path is HOME
  if (tools::file_path_as_absolute(input_dir) == path.expand("~")) {
    message("You probably run strip_device_id() by mistake on the entire Home directoy. Check input_dir carefully\n",
            "Current directory set to: ", input_dir, '\n')
    answer <- readline("\nProceed now? [yes/no]: ")
    answer <- tolower(trimws(answer))
    if (answer == 'no') {
      stop("Processing skipped")
    } else if (answer == 'yes') {
      message('Processing continues\n')
    } else {
      stop(answer, ' is not an expected answer. Quit')
    }
  }

    ## Retrieve date & time
    files_new <- substr(wave_files, nchar(wave_files) - 14 - nchar(pattern), nchar(wave_files))

    if (isTRUE(.simulate)) {
      cat("Preview of file renaming!")
      x <- data.frame(old = basename(wave_files),
                      new = files_new)
      utils::head(x)
    } else {
      ## compare strings
      if (all(identical(basename(wave_files), files_new))) {
        cat("Files already named correctly!")
      } else {
        x <- file.rename(from = file.path(dirname(wave_files), basename(wave_files)),
                         to = file.path(dirname(wave_files), files_new))
      }
    }
  }

  #' read audacity labels
  #'
  #' @param path path
  #' @param pattern pattern to recognise
  #' @param delim delim
  #' @importFrom readr read_delim
  #' @importFrom pbapply pblapply
  #' @export
  #'
  read_audacity <- function(path, pattern = c('BirdNET.results.txt', 'BirdNET.labels.txt'), delim = '\t') {
    pattern <- match.arg(pattern)

    if (delim == '\t') {
      if (pattern == 'BirdNET.results.txt') {
        labs <- c("start", "end", "label", "score")
      } else if (pattern == 'BirdNET.labels.txt') {
        labs <- c("start", "end", "label")
      }
    } else {
      labs <- FALSE
    }

    files <- list.files(path, recursive = T, pattern = pattern, full.names = T)

    if (length(files) >= 1) {
      # read audacity marks
      cat("Read audacity marks:\n")
      results <- pbapply::pblapply(files,
                                   readr::read_delim,
                                   delim = delim,
                                   progress = FALSE,
                                   col_names = labs,
                                   show_col_types = FALSE)
      return(do.call('rbind',results))
    }
  }

  #' splits species names
  #'
  #' @description
  #' Split BirdNET species name in scientific and common name. If model perch v2 is used, try to lookup common names from BirdNET species list.
  #'
  #' @param x string
  #' @inheritParams birdNET_process
  #' @keywords internal
  split_species_names <- function(x, model = c('BirdNET v2.4', 'Perch v2')) {
    model <- match.arg(model)
    if (model == 'BirdNET v2.4') {
      parts <- strsplit(x, "_")
      df <- as.data.frame(do.call(rbind, parts), stringsAsFactors = FALSE)
      names(df) <- c("scientific_name", "common_name")
    } else if (model == 'Perch v2') {
      slist <- read_birdnet_slist(.cached = T, model = model)[,c("name_scientific", "name_de")]
      names(slist) <- c("scientific_name", "common_name")
      df <- dplyr::left_join(
        data.frame(scientific_name = x),
        slist,
        by = 'scientific_name')
    }
    return(df)
  }

  #' Split wave files in 10 minute segments
  #' @description
  #' Split long wave files in segments and overwrite input files. For safety reasons,
  #' files are first saved to a directory \code{temp} before replacing the input.
  #'
  #'
  #' @param path path
  #' @param pattern file format, .wav or .mp3
  #' @export
  #'
  split_waves <- function(path, pattern = c('.wav', '.mp3')) {
    pattern <- match.arg(pattern)
    ## get input files
    waves <- list.files(path = path,
                        pattern = pattern,
                        ignore.case = T,
                        recursive = T,
                        full.names = T)

    # Filter out any files from the folder "extracted"
    waves <- waves[!grepl("extracted/", waves)]

    ## check if recording length > 600
    rec.length <- sapply(waves, function(wav) {
      audio <- tuneR::readWave(filename = wav, header = TRUE)
      sec <- audio$samples / audio$sample.rate
      return(sec)
    })

    if (all(rec.length <= 600)) {
      message('All ', length(waves), ' files at max 600 sec. Skip split waves.')
    } else {

      if (interactive()) message('* ', length(waves), ' audio files to process\n')

      ## create TempDir
      TempDir <- file.path(path, 'TempDir')
      dir.create(TempDir, showWarnings = T)

      out <- lapply(1:length(waves), function(x) {
        message('* Process ', basename(waves[x]), '\n')

        ## read header of waves
        audio <- tuneR::readWave(filename = waves[x], header = TRUE)
        ## estimate length in seconds
        sec <- audio$samples / audio$sample.rate
        ## define breaks to write audio chunks (keep unique) if
        ## last is identical to duration
        breaks <- unique(c(seq(from = 0, to = sec, by = 600), sec))
        ## get time from file name
        head <- lubridate::make_datetime(
          year = as.numeric(substr(basename(waves[x]), 1, 4)),
          month = as.numeric(substr(basename(waves[x]), 5, 6)),
          day = as.numeric(substr(basename(waves[x]), 7, 8)),
          hour = as.numeric(substr(basename(waves[x]), 10, 11)),
          min =  as.numeric(substr(basename(waves[x]), 12, 13)),
          sec = as.numeric(substr(basename(waves[x]), 14, 15)))

        ## define segments
        df <- data.frame(
          ctime = head,
          from = breaks[1:(length(breaks) - 1)],
          to = breaks[-1],
          seconds = diff(breaks),
          file = basename(waves[x]),
          path = path,
          TempDir = TempDir)

        ## adjust times for date_time label as header
        if (nrow(df) > 1) {
          for (i in 2:nrow(df)) {
            df[i, "ctime"] <- df[i - 1, "ctime"] + df[i - 1, "seconds"]
          }
        }

        ## create file names based on ctime of recordings
        df[["new.name"]] <- paste0(format(df[["ctime"]], "%Y%m%d_%H%M%S"),toupper(pattern))

        message("extract segments")
        silent <- lapply(1:nrow(df), function(i) {
          ## read audio
          audio <- tuneR::readWave(filename = waves[x],
                                   from = df[i, "from"],
                                   to = df[i, "to"],
                                   units = "seconds")
          #suppressWarnings(tuneR::writeWave(audio, filename = file.path(path, df[i, "new.name"])))
          suppressWarnings(tuneR::writeWave(audio, filename = file.path(TempDir, df[i, "new.name"])))
          rm(audio)
          gc(full = T, verbose = F)
        })
        ## copy from TempDir back to path
        file.copy(from = file.path(df[["TempDir"]], df[["new.name"]]),
                  to = file.path(df[["path"]], df[["new.name"]]),
                  overwrite = T)
      })
      unlink(TempDir, recursive = T)
    }
  }

  #' Interactively check, load and optionally install missing R packages
  #'
  #' @param pkgs Character vector of package names to check/install.
  #' @export
  #'
  .check_pkgs <- function(pkgs) {

    pkgs_missing <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
    pkgs_available <- pkgs[sapply(pkgs, requireNamespace, quietly = TRUE)]

    if (length(pkgs_missing) > 0) {

      message("\nThe following packages are not installed:")
      message(paste0("  - ", pkgs_missing, collapse = "\n"))
      answer <- readline("\nInstall now? [y/n]: ")

      if (!tolower(trimws(answer)) %in% c("y", "yes")) {
        message("Installation skipped.")

      } else if (tolower(trimws(answer)) %in% c("y", "yes")) {
        results <- lapply(pkgs_missing, function(pkg) {
          message("\nInstalling: ", pkg)
          utils::install.packages(pkg, quiet = TRUE)
        })
        pkgs_available <- pkgs[sapply(pkgs, requireNamespace, quietly = TRUE)]

      }
    }
    invisible(lapply(pkgs_available, library, character.only = TRUE))
  }
