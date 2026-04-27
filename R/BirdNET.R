#' Daily activity graph
#'
#' @param taxon Taxon name
#' @param path path
#' @import ggplot2
#' @import dplyr
#' @importFrom lubridate hour
#' @importFrom readxl read_xlsx
#' @importFrom tidyr complete
#' @export
#'
birdNET_graph <- function(path, taxon) {

  Taxon <- T1 <- hh <- NA
  ## Load and filter dataset
  df <- readxl::read_xlsx(file.path(path, "BirdNET.xlsx")) %>%
    dplyr::filter(Taxon == taxon) %>%
    dplyr::mutate(hh = lubridate::hour(T1))

  ## get plot dimensions
  max_n <- df |> count(hh) |> pull(n) |> max()
  label_y <- max_n * 1.08

  ## create plot
  plot <- df |>
    count(hh) |>
    tidyr::complete(hh = 0:23, fill = list(n = 0)) |>
    ggplot(aes(x = hh, y = n)) +
    geom_bar(stat = 'identity', col = "black", fill = "darkorange") +
    scale_x_continuous(breaks = 0:23, expand = c(0, 0)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
    labs(
      title    = taxon,
      x = "Hour",
      y = "Events",
      caption = paste("BirdNET v2.4", 'Detektionen:', nrow(df), '\n', min(df$T1), '-', max(df$T1))
    ) +
    theme_minimal(base_size = 13) +
    theme(panel.grid = element_blank())
  return(plot)
}


#' Export birdnetR predictions to audacity label file
#'
#' @description
#' Creates a text file with labels for use in Audacity matching --rtype 'audacity' when using [BirdNET-Analyzer](https://github.com/birdnet-team/BirdNET-Analyzer)
#'
#' @param predictions list of data frames returned by \link[birdnetR]{predict_species_from_audio_file }
#' @param wave_files path to wave_files in order of data frames
#' @importFrom readr write_delim
#' @importFrom stringr str_replace
#' @importFrom tools file_ext
#' @importFrom stats na.omit
#' @importFrom methods is
#'
#' @export
#'
birdnet2audacity <- function(predictions, wave_files) {

  ## if predictions is a list ...
  if (methods::is(predictions, 'list')) {
    for (i in 1:length(predictions)) {
      ## select list entry number i
      df <- predictions[[i]]
      ## check if there is at least one prediction or skip
      if (nrow(df) > 1) {
        ## tweak label
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
                                      replacement = "BirdNET.results.txt"),
          col_names = F)

      }
    }
  } else if (methods::is(predictions, 'data.frame')) {
    df <- predictions
    if (nrow(df) > 1) {
      ## tweak label
      df <- data.frame(start = df$start,
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
                                    replacement = "BirdNET.results.txt"),
        col_names = F)

    }
  }
}

#' Execute \link[birdnetR]{predict_species_from_audio_file } with custom settings
#' @param audio audio file
#' @param slist species_list as character vector
#' @param language birdnetR::predict_species_from_audio_file
#' @param batch_size see \link[birdnetR]{predict_species_from_audio_file }
#' @param min_confidence see \link[birdnetR]{predict_species_from_audio_file }
#' @param chunk_overlap_s see \link[birdnetR]{predict_species_from_audio_file }
#' @param sigmoid_sensitivity see \link[birdnetR]{predict_species_from_audio_file }
#' @return see \link[birdnetR]{predict_species_from_audio_file }
#' @importFrom birdnetR predict_species_from_audio_file
#' @importFrom birdnetR birdnet_model_tflite
#' @export
#'
birdNET_process <- function(
    audio,
    slist = NULL,
    language = 'de',
    batch_size = 1,
    min_confidence = 0.7,
    chunk_overlap_s = 0,
    sigmoid_sensitivity = 1.25) {

  results <- birdnetR::predict_species_from_audio_file(
    batch_size = as.integer(batch_size),
    audio_file = audio,
    model = birdnetR::birdnet_model_tflite(language = language),
    min_confidence = min_confidence,
    chunk_overlap_s = chunk_overlap_s,
    sigmoid_sensitivity = sigmoid_sensitivity,
    filter_species = slist,
    keep_empty = F)
  return(results)
}

#' Split execution of \link{birdNET_process} in sets of wave files
#'
#' @inheritParams birdNET_process
#' @param wave_files audio files to process
#' @param n number of audio files to process in a single run
#' @param restart optional. restart iteration from here
#' @export
#'
birdNET_process_batch <- function(
    wave_files,
    n = 12,
    slist = NULL,
    language = 'de',
    batch_size = 3,
    restart = NULL,
    min_confidence = 0.7,
    chunk_overlap_s = 0,
    sigmoid_sensitivity = 1.25) {

  if (!is.null(restart)) {
    wave_files <- wave_files[restart:length(wave_files)]
  }

  ## establish number of iterations
  chunks <- unique(c(seq(0, length(wave_files), n), length(wave_files)))

  # Create a progress bar
  # pb <- utils::txtProgressBar(min = 0, max = length(chunks), style = 3)
  pb <- utils::txtProgressBar(min = 0, max = length(wave_files), style = 3)

  cat(paste0('Start: ',as.character(strftime(Sys.time(),format = "%Y-%m-%d %H:%M:%S")),'\n\n'))
  ## for each chunk ...
  x <- lapply(1:(length(chunks) - 1), function(chunk) {
    predictions <- lapply((chunks[chunk] + 1):chunks[chunk + 1], function(x) {
      cat('Process recording', x, 'out of', max(chunks), "\n")
      utils::setTxtProgressBar(pb, x)
      cat("\n")
      birdNET_process(audio = wave_files[[x]],
                      language = 'de',
                      slist = slist,
                      batch_size = batch_size,
                      min_confidence = min_confidence,
                      chunk_overlap_s = chunk_overlap_s,
                      sigmoid_sensitivity = sigmoid_sensitivity)
    })
    ## export audacity labels
    birdnet2audacity(predictions = predictions, wave_files = wave_files[chunks[chunk]:chunks[chunk + 1]])
    return(predictions)
  })

  # Close the progress bar
  close(pb)
  cat(paste0('Finish: ',as.character(strftime(Sys.time(),format = "%Y-%m-%d %H:%M:%S")),'\n\n'))
  ## free up disk space
  gc(verbose = FALSE)
  return(x)
}


#' Read and subset species list from BirdNET v2.4 model
#'
#' @param language language. defaults to 'de'
#' @export
#'
read_birdnet_slist <- function(language = 'de') {
  ## read model
  model = birdnetR::birdnet_model_tflite(language = language)
  ## read labels
  slist <- birdnetR::read_labels(birdnetR::labels_path(model, language = language))

  ## format slist
  slist <- lapply(slist, function(name) {
    out <- stringr::str_split(name, "_")
    data.frame(
      name = name,
      name_scientific = out[[1]][1],
      name_de = out[[1]][2])
  })
  slist <- do.call("rbind", slist)
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
write_birdnet_slist <- function(sci_names = c('Bubo bubo', 'Buteo butep'), filename = NULL, language = 'de') {

  name_scientific <-

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

