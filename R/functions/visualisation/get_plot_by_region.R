get_plot_by_region <- function(data_source, sel_region) {
  data_source %>%
    dplyr::filter(region == sel_region) %>%
    purrr::chuck("plot", 1) %>%
    return()
}