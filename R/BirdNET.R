#' Daily activity graph
#'
#' @param taxon Taxon name
#' @param path path
#' @param model model used to classifiy
#' @import ggplot2
#' @import dplyr
#' @importFrom lubridate hour
#' @importFrom readxl read_xlsx
#' @importFrom tidyr complete
#' @importFrom egg  theme_presentation
#' @export
#'
birdNET_graph <- function(path, taxon, model = c("BirdNET_V2.4", "Perch v2")) {

  model <- match.arg(model)

  Taxon <- T1 <- hh <- Verification <- NA
  ## Load and filter dataset
  df <- readxl::read_xlsx(file.path(path, "BirdNET.xlsx")) %>%
    dplyr::filter(Taxon == taxon) %>%
    dplyr::mutate(
      hh = lubridate::hour(T1),
      Verification = dplyr::case_when(
        Verification == TRUE | tolower(Verification) == "t" ~ "Verified",
        Verification == FALSE | tolower(Verification) == "f" ~ "Rejected",
        .default = "Not verified"
      )
    )

  ## get plot dimensions
  max_n <- df |> count(hh) |> pull(n) |> max()
  label_y <- max_n * 1.08

  ## create plot
  plot <- df |>
    count(hh, Verification) |>
    tidyr::complete(hh = 0:23, Verification, fill = list(n = 0)) |>
    ggplot(aes(x = hh, y = n, fill = Verification)) +
    geom_bar(stat = 'identity', col = "black") +
    scale_x_continuous(breaks = 0:23, expand = c(0, 0)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
    scale_fill_manual(
      values = c(Verified = "#4daf4a",
                 Rejected = "#e41a1c",
                 "Not verified" = "#377eb8"),
      guide = guide_legend(title = "Verification")
    ) +
    labs(
      title    = taxon,
      x = "Hour",
      y = "Events",
      caption = paste0(nrow(df), ' events', ' (', model, ')', '\n', min(df$T1), '-', max(df$T1))
    ) +
    egg::theme_presentation() +
    theme(panel.grid = element_blank())
  return(plot)
}


#' Export birdnetR predictions to audacity label file
#'
#' @description
#' Creates a text file with labels for use in Audacity matching --rtype 'audacity' when using [BirdNET-Analyzer](https://github.com/birdnet-team/BirdNET-Analyzer)
#'
#' @param predictions list of data frames returned by birdnetR
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
        df <- cbind(df, split_species_names(df[["species_name"]]))
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
    if (nrow(predictions) > 1) {

      df <- cbind(predictions, split_species_names(predictions[["species_name"]]))

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
                                    replacement = "BirdNET.results.txt"),
        col_names = F)

    }
  }
}

#' Execute \link[=predict.birdnet_model_acoustic]{birdnet.predict()} with custom settings
#'
#' @description
#' Runs \link[=predict.birdnet_model_acoustic]{birdnet.predict()} on audio files
#'
#' @param audio audio file
#' @param model one of c('BirdNET v2.4', 'Perch v2')
#' @param slist see \code{species_list} in \link[=predict.birdnet_model_acoustic]{birdnet.predict()}
#' @param language see \link[=predict.birdnet_model_acoustic]{birdnet.predict()}
#' @param batch_size see \link[=predict.birdnet_model_acoustic]{birdnet.predict()}
#' @param min_confidence see \link[=predict.birdnet_model_acoustic]{birdnet.predict()}
#' @param chunk_overlap_s see \link[=predict.birdnet_model_acoustic]{birdnet.predict()}
#' @param sigmoid_sensitivity see \link[=predict.birdnet_model_acoustic]{birdnet.predict()}
#' @return see \link[=predict.birdnet_model_acoustic]{birdnet.predict()}
#' @importFrom birdnetR load_birdnet
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
    stop('Perch v2 not yet working')
    model = birdnetR::load_perch()
    # └─birdnetR::load_perch()
    # 2.   └─py_birdnet$load_perch_v2("CPU")
    # 3.     └─reticulate:::py_call_impl(callable, call_args$unnamed, call_args$named)
    # See `reticulate::py_last_error()$r_trace$full_call` for more details.
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
    model = c('BirdNET v2.4', 'Perch v2'),
    n = 12,
    slist = NULL,
    language = 'de',
    batch_size = 3,
    restart = NULL,
    min_confidence = 0.7,
    chunk_overlap_s = 0,
    sigmoid_sensitivity = 1.25) {

  .Deprecated("run_birdnet")

  # upgrading to birdnetR 1.0 [2026-05-29] -------------------
  model <- match.arg(model)

  if (!is.null(restart)) {
    wave_files <- wave_files[restart:length(wave_files)]
  }

  ## establish number of iterations
  chunks <- unique(c(seq(0, length(wave_files), n), length(wave_files)))

  # Create a progress bar
  pb <- utils::txtProgressBar(min = 0, max = length(wave_files), style = 3)

  cat(paste0('Start: ',as.character(strftime(Sys.time(),format = "%Y-%m-%d %H:%M:%S")),'\n\n'))
  ## for each chunk ...
  x <- lapply(1:(length(chunks) - 1), function(chunk) {
    predictions <- lapply((chunks[chunk] + 1):chunks[chunk + 1], function(x) {
      cat('Process recording', x, 'out of', max(chunks), "\n")
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
    birdnet2audacity(
      predictions = predictions,
      wave_files = wave_files[chunks[chunk]:chunks[chunk + 1]])
    return(predictions)
  })

  # Close the progress bar
  close(pb)
  cat(paste0('Finish: ',as.character(strftime(Sys.time(),format = "%Y-%m-%d %H:%M:%S")),'\n\n'))
  ## free up disk space
  gc(verbose = FALSE)
  return(x)
}


#' Read and subset species list from models
#'
#' @inheritParams birdNET_process
#' @param language language. defaults to 'de'
#' @param .cached logical
#' @export
#'
read_birdnet_slist <- function(
    model = c('BirdNET v2.4'),
    language = 'de',
    .cached = F) {
  # upgrading to birdnetR 1.0 [2026-05-29] -------------------
  if (isFALSE(.cached)) {
    model <- match.arg(model)

    ## read model
    if (model == 'BirdNET v2.4') {
      model = birdnetR::load_birdnet(type = 'acoustic', version = '2.4', language = language)
    }
    ## read labels
    slist <- birdnetR::get_species_list(model)

    ## format slist
    slist <- lapply(slist, function(name) {
      out <- stringr::str_split(name, "_")
      data.frame(
        name = name,
        name_scientific = out[[1]][1],
        name_de = out[[1]][2])
    })
    slist <- do.call("rbind", slist)
  } else if (isTRUE(.cached)) {
    slist <- readr::read_delim(system.file("extdata/BirdNET_v2.4.txt", package = "MonitoR"),
                               delim = " ",
                               col_names = c('name', 'name_scientific', 'name_de'),
                               show_col_types = F)
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
#' @export
#'
birdNET_select <- function(
    path = NULL,
    model = c('BirdNET v2.4', 'Perch v2'),
    slist = system.file("extdata/ornitho_de.txt", package = "MonitoR")) {
  Taxon <- NULL
  model <- match.arg(model)

  ## 'BirdNET v2.4'
  if (model == 'BirdNET v2.4') {
    mlist <- read_birdnet_slist(.cached = T)
    slist <- readr::read_delim(slist, delim = "_", col_names = c('name_scientific', 'name_de'), show_col_types = F)
    slist[['name']] <- paste0(slist[["name_scientific"]], '_', slist[["name_de"]])
    names_not_matched <- slist[["name"]][!slist[["name"]] %in% mlist[["name"]]]
    if (length(names_not_matched)) warning('Not all species names found in model', model)
  }

  ## read and filter Records
  Records <- readxl::read_xlsx(file.path(path, "BirdNET.xlsx"))

  before <- nrow(Records)
  Records <- dplyr::filter(Records, Taxon %in% slist[["name_de"]])
  after <- nrow(Records)
  message('Filtered Records: From ', before, ' to ', after)

  Meta <- readxl::read_xlsx(file.path(path, "BirdNET.xlsx"), sheet = 'Meta')
  out <- list(
    Records = Records,
    #Records.dd = BirdNET_table$records.day,
    #Records.hh = BirdNET_table$records.hour,
    Meta = Meta)
  openxlsx::write.xlsx(x = out,
                       file = file.path(path, "BirdNET.xlsx"), overwrite = T)
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

  if (isTRUE(skip.existing.results)) {
    ## build BirdNET.results files
    results_files <- stringr::str_replace(
      string = wave_files,
      pattern = tools::file_ext(wave_files),
      replacement = 'BirdNET.results.txt')
    ## check if exist
    indices <- sapply(results_files, file.exists)
    ## filter wave files
    wave_files <- wave_files[!indices]
  }

  # Create a progress bar
  pb <- utils::txtProgressBar(min = 0, max = length(wave_files), style = 3)

  cat(paste0('Start: ',as.character(strftime(Sys.time(),format = "%Y-%m-%d %H:%M:%S")),'\n\n'))
  ## for each chunk ...
  predictions <- lapply(1:length(wave_files), function(x) {
    cat('Process recording', x, 'out of', length(wave_files), "\n")
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
  birdnet2audacity(
    predictions = predictions,
    wave_files = wave_files)
  return(predictions)


  # Close the progress bar
  close(pb)
  cat(paste0('Finish: ',as.character(strftime(Sys.time(),format = "%Y-%m-%d %H:%M:%S")),'\n\n'))
  ## free up disk space
  gc(verbose = FALSE)
  return(x)
}
