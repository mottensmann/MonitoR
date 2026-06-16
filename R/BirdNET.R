#' Export birdnetR predictions to audacity label file
#'
#' @description
#' Creates a text file with labels for use in Audacity matching --rtype 'audacity' when using [BirdNET-Analyzer](https://github.com/birdnet-team/BirdNET-Analyzer)
#'
#' @param predictions list of data frames returned by birdnetR
#' @param wave_files path to wave_files in order of data frames
#' @inheritParams birdNET_process
#' @importFrom readr write_delim
#' @importFrom stringr str_replace
#' @importFrom tools file_ext
#' @importFrom stats na.omit
#' @export
#'
birdnet2audacity <- function(predictions, wave_files, model = c('BirdNET v2.4', 'Perch v2')) {

  ## set output names depending on model
  model <- match.arg(model)
  if (model == 'BirdNET v2.4') {
    my_replacement = "BirdNET.results.txt"
  } else if (model == 'Perch v2') {
    my_replacement = "Perch.results.txt"
  }

  ## if predictions is a list ...
  if (methods::is(predictions, 'list')) {
    for (i in 1:length(predictions)) {
      ## select list entry number i
      df <- predictions[[i]]
      ## check if there is at least one prediction or skip
      if (nrow(df) > 1) {
        ## tweak label
        df <- cbind(df, split_species_names(df[["species_name"]], model = model))
        df <- data.frame(start = df$start,
                         end = df$end,
                         name = paste0(df$scientific_name, ", ", df$common_name),
                         score = df$confidence)
        ## write to file
        readr::write_delim(
          delim = "\t",
          quote = 'none',
          x = df,
          file = stringr::str_replace(string = wave_files[i],
                                      pattern = tools::file_ext(wave_files[i]) ,
                                      replacement = my_replacement),
          col_names = F)

      }
    }
  } else if (methods::is(predictions, 'data.frame')) {
    if (nrow(predictions) > 1) {

      df <- cbind(predictions, split_species_names(predictions[["species_name"]], model = model))

      ## tweak label
      df <- data.frame(
        start = df$start,
        end = df$end,
        name = paste0(df$scientific_name, ", ", df$common_name),
        score = df$confidence)

      ## write to file
      readr::write_delim(
        delim = "\t",
        quote = 'none',
        x = df,
        file = stringr::str_replace(string = wave_files,
                                    pattern = tools::file_ext(wave_files) ,
                                    replacement = my_replacement),
        col_names = F)

    }
  }
}

#' Runs \code{birdnetR::predict.birdnet_model_acoustic} with custom settings
#'
#' @description
#' Runs \code{birdnetR::predict.birdnet_model_acoustic} with custom settings on audio file.
#'
#' @param audio Audio file to process.
#' @param model One of \code{c('BirdNET v2.4', 'Perch v2')}
#' @param slist Optional. Character vector or list of species names to filter the predictions
#' @param language Character. defaults to \code{'de'}
#' @param batch_size Optional. An integer specifying the number of audio segments evaluated per inference call.
#' @param min_confidence Numeric value to set the minimum confidence threshold for predictions.
#' @param chunk_overlap_s Numeric value to set the overlap between consecutive segments.
#' @param sigmoid_sensitivity Numeric value that adjusts the sensitivity of the sigmoid function. Must be in the interval [0.5, 1.5]. defaults to \code{1.25}
#' @return An S3 object of class \code{birdnet_prediction_acoustic} and \code{birdnet_prediction} containing the prediction results
#' @importFrom birdnetR load_birdnet
#' @seealso \code{\link[birdnetR:predict.birdnet_model_acoustic]{birdnet.predict()}}
#' @export
#'
birdNET_process <- function(
    audio,
    model = c('BirdNET v2.4', 'Perch v2'),
    slist = NULL,
    language = 'de',
    batch_size = NULL,
    min_confidence = 0.7,
    chunk_overlap_s = 0,
    sigmoid_sensitivity = 1.25) {

  # upgrading to birdnetR 1.0 [2026-05-29] -------------------

  ### select model -------------------------------------------
  model <- match.arg(model)
  if (model == 'BirdNET v2.4') {
    model = birdnetR::load_birdnet(type = 'acoustic', version = '2.4', language = language)
  } else if (model == 'Perch v2') {
    model = birdnetR::load_perch()
  }

  ## Predict species -----------------------------------------
  results <- stats::predict(
    object = model,
    files = tools::file_path_as_absolute(audio),
    min_confidence = min_confidence,
    chunk_overlap_s = chunk_overlap_s,
    sigmoid_sensitivity = sigmoid_sensitivity,
    top_k = 1,
    species_list = slist,
    batch_size = batch_size)

  ## return results ------------------------------------------
  return(as.data.frame(results))
}

#' Read and subset species list from models
#'
#' @inheritParams birdNET_process
#' @param language language. defaults to 'de'
#' @param .cached logical
#' @export
#'
read_birdnet_slist <- function(
    model = c('BirdNET v2.4', 'Perch v2'),
    language = 'de',
    .cached = F) {

  model <- match.arg(model)
  # upgrading to birdnetR 1.0 [2026-05-29] -------------------
  if (isFALSE(.cached)) {
    ## read model
    if (model == 'BirdNET v2.4') {
      selected_model = birdnetR::load_birdnet(type = 'acoustic', version = '2.4', language = language)
    } else if (model == 'Perch v2') {
      selected_model = birdnetR::load_perch()
    }
    ## read labels
    slist <- birdnetR::get_species_list(selected_model)

    if (model == 'BirdNET v2.4') {
      ## format slist
      slist <- lapply(slist, function(name) {
        out <- stringr::str_split(name, "_")
        data.frame(
          name = name,
          name_scientific = out[[1]][1],
          name_de = out[[1]][2])
      })
      slist <- do.call("rbind", slist)
    } else if (model == 'Perch v2') {
      slist <- data.frame(
        name = slist,
        name_scientific = slist,
        name_de = NA)
    }
  } else if (isTRUE(.cached)) {
    if (model == 'BirdNET v2.4') {
      slist <- readr::read_delim(system.file("extdata/BirdNET_v2.4.txt", package = "MonitoR"),
                                 delim = " ",
                                 col_names = c('name', 'name_scientific', 'name_de'),
                                 show_col_types = F)

    } else if (model == 'Perch v2') {
      slist <- readr::read_delim(system.file("extdata/Perch_v2_slist.txt", package = "MonitoR"),
                                 delim = " ",
                                 col_names = c('name', 'name_scientific', 'name_de'),
                                 show_col_types = F)
    }

  }
  return(slist)
}

#' Write custom species list
#' @param sci_names character vector of scientific species names
#' @param filename name of text file (e.g. 'my_folder/custom_species_list.txt')
#' @param language language. defaults to 'de'
#' @export
#' @examples
#' # NOT RUN
#' ## Read species list of ornitho.de
#' # sci_names <- readxl::read_xlsx('~/Artenlisten/Avifauna.xlsx', 'OrnithoV3', 'D7:D798', 'Names')
#' ## Write species list to file
#' # out <- write_birdnet_slist(names[['Names']], "inst/extdata/ornitho_de.txt")
#'
write_birdnet_slist <- function(
    sci_names = c('Bubo bubo', 'Buteo buteo'),
    filename = NULL,
    language = 'de') {

  name_scientific <- NA

  ## read labels from birdnet model
  slist <- read_birdnet_slist(language = language)

  ## filter based on sci_names
  slist_custom <- dplyr::filter(slist, name_scientific %in% sci_names)

  ## export list
  if (is.null(filename)) {
    warning('Export custom species list to: ', file.path(getwd(), "custom_species_list.txt"))
    filename <- "custom_species_list.txt"
  }
  ## check for names not resolved
  slist_error <- sci_names[!sci_names %in% slist[['name_scientific']]]

  if (length(slist_error) > 1) {
    out <- list(slist_custom = slist_custom, error = data.frame(name = slist_error), filename = filename)
  } else {
    out <- list(slist_custom = slist_custom, filename = filename)
  }
  utils::write.table(x = slist_custom[["name"]], file = filename, row.names = F, col.names = F, quote = F)
  return(out)
}

#' Filter BirdNET selection tables by species
#'
#' @param path path to results
#' @param slist path to custom species list
#' @param model one of 'BirdNET v2.4' or 'Perch v2'
#' @importFrom dplyr filter
#' @importFrom NocMigR2 reformat_xlsx
#' @export
#'
birdNET_select <- function(
    path = NULL,
    model = c('BirdNET v2.4', 'Perch v2'),
    slist = system.file("extdata/ornitho_de.txt", package = "MonitoR")) {
  Taxon <- NULL
  model <- match.arg(model)

  if (model ==        'BirdNET v2.4') {
    xlsx <-           'BirdNET.xlsx'

  } else if (model == 'Perch v2') {
    xlsx <-           'Perch.xlsx'
  }

  ## 'BirdNET v2.4'
  mlist <- read_birdnet_slist(.cached = T, model = model)
  slist <- readr::read_delim(slist, delim = "_", col_names = c('name_scientific', 'name_de'), show_col_types = F)
  slist[['name']] <- paste0(slist[["name_scientific"]], '_', slist[["name_de"]])
  names_not_matched <- slist[["name"]][!slist[["name"]] %in% mlist[["name"]]]
  if (length(names_not_matched)) warning('Not all species names found in model', model)

  ## read and filter Records
  Records <- readxl::read_xlsx(file.path(path, xlsx))

  before <- nrow(Records)
  Records <- dplyr::filter(Records, Taxon %in% slist[["name_de"]])
  after <- nrow(Records)
  message('Filtered Records: From ', before, ' to ', after)

  Meta <- readxl::read_xlsx(file.path(path, xlsx), sheet = 'Meta')
  out <- list(
    Records = Records,
    #Records.dd = BirdNET_table$records.day,
    #Records.hh = BirdNET_table$records.hour,
    Meta = Meta)
  openxlsx::write.xlsx(x = out,
                       file = file.path(path, xlsx), overwrite = T)
  NocMigR2::reformat_xlsx(path = path)
  return(Records)
}

#' Split execution of \link{birdNET_process} in sets of wave files
#'
#' @description
#' Replaces \link{birdNET_process_batch}
#'
#' @inheritParams birdNET_process
#' @param wave_files audio files to process
#' @param skip.existing.results boolean
#' @export
#'
run_birdnet <- function(
    wave_files,
    model = c('BirdNET v2.4', 'Perch v2'),
    slist = NULL,
    language = 'de',
    batch_size = 3,
    min_confidence = 0.7,
    chunk_overlap_s = 0,
    sigmoid_sensitivity = 1.25,
    skip.existing.results = FALSE) {

  x <- NULL
  model <- match.arg(model)

  if (model == 'BirdNET v2.4' & isTRUE(skip.existing.results)) {
    ## build BirdNET.results files
    results_files <- stringr::str_replace(
      string = wave_files,
      pattern = tools::file_ext(wave_files),
      replacement = 'BirdNET.results.txt')
    ## check if exist
    indices <- sapply(results_files, file.exists)
    ## filter wave files
    wave_files <- wave_files[!indices]
    if (length(wave_files) == 0) stop('All recordings analysed. Set skip.existing.results = FALSE to rerun')
  } else if (model == 'Perch v2' & isTRUE(skip.existing.results)) {
    ## build Perch.results files
    results_files <- stringr::str_replace(
      string = wave_files,
      pattern = tools::file_ext(wave_files),
      replacement = 'Perch.results.txt')
    ## check if exist
    indices <- sapply(results_files, file.exists)
    ## filter wave files
    wave_files <- wave_files[!indices]
    if (length(wave_files) == 0) stop('All recordings analysed. Set skip.existing.results = FALSE to rerun')
  }

  # Create a progress bar
  pb <- utils::txtProgressBar(min = 0, max = length(wave_files), style = 3)

  cat(paste0(
    "\n  {o,o}",
    "\n  |)  )",
    "\n  -'-'-",
    "\nStart: ", strftime(Sys.time(), format = "%Y-%m-%d %H:%M:%S"),
    " Model: ", model, "\n\n"
  ))

  ## for each chunk ...
  predictions <- lapply(1:length(wave_files), function(x) {
    cat("\014")
    cat(paste0(
      "\n  {o,o}",
      "\n  |)  )",
      "\n  -'-'-",
      '\nProcess ', x, ' out of ', length(wave_files), ' (', model, ')', ' [', strftime(Sys.time(), format = "%Y-%m-%d %H:%M:%S"), ']', "\n"
    ))
    utils::setTxtProgressBar(pb, x)
    cat("\n")
    birdNET_process(
      model = model,
      audio = wave_files[[x]],
      language = 'de',
      slist = slist,
      batch_size = batch_size,
      min_confidence = min_confidence,
      chunk_overlap_s = chunk_overlap_s,
      sigmoid_sensitivity = sigmoid_sensitivity)
  })
  ## export audacity labels
  birdnet2audacity(predictions = predictions,
                   wave_files = wave_files,
                   model = model)
  return(predictions)

  # Close the progress bar
  close(pb)
  cat(paste0('Finish: ',as.character(strftime(Sys.time(),format = "%Y-%m-%d %H:%M:%S")),'\n\n'))
  ## free up disk space
  gc(verbose = FALSE)
  return(x)
}
