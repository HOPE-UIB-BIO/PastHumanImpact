#' @title Get data from path
#' @return Data
get_file_from_path <- function(path) {
  readr::read_rds(path) %>%
    return()
}
