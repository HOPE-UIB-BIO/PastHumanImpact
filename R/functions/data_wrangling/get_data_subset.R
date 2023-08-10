#' @description This is a dummy function to tets functionality of branching
#' pipeline
get_data_subset <- function(data_source, sel_group) {
  Sys.sleep(5)

  data_source %>%
    dplyr::filter(group == sel_group) %>%
    return()
}
