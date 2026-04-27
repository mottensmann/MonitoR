#' Read summary.txt
#'
#' @param path path
#' @param pattern defaults to 'Summary.txt'
#' @export
#'
read_params <- function(path, pattern = 'Summary.txt') {
  df <- readr::read_delim(file.path(path, list.files(path, pattern = pattern)))
  return(df)
}

