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
