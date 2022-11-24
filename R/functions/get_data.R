#' @title Get pollen data from data assembly
#' @description Use data assembly derived from RFossilPol
#' @return Data assembly
get_data <- function(path) {
  get_file_from_path(path) %>%
    purrr::pluck("data")
}
