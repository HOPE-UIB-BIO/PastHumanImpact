add_age_as_factor <- function(data_source) {
  data_source %>%
    dplyr::mutate(
      age = age / 1e3,
      age = factor(
        age,
        levels = seq(8.5, 0, -0.5)
      )
    ) %>%
    return()
}