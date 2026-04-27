#' Read BirdNET_analysis_params
#'
#' @param path path
#' @param pattern defaults to 'BirdNET_analysis_params.csv'
#' @export
#'
read_BirdNET_analysis_params <- function(path, pattern = 'BirdNET_analysis_params.csv') {
  df <- readr::read_csv(file.path(path, pattern))
  return(df)
}

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
#' Use with caution as the functions simply retrieves the last 14 chacters of the file name. If unsure set \code{.simulate = TRUE}. Folders named 'extracted' are skipped!
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
#' @param delim delimeter
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
