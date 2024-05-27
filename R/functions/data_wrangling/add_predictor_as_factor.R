add_predictor_as_factor <- function(data_source) {
  data_source %>%
    dplyr::mutate(
      predictor = factor(
        predictor,
        levels = c("human", "climate")
      )
    ) %>%
    return()
}
