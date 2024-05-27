add_region_as_factor <- function(data_source) {
  data_source %>%
    dplyr::mutate(
      region = factor(
        region,
        levels = vec_regions # [config criteria]
      )
    ) %>%
    return()
}
