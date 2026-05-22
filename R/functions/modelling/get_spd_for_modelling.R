#' @title Prepare SPD data for modelling
#' @description
#' Unnest SPD tables, pivot to long format, round values, and nest modelling
#' data by variable.
#' @param data_source_spd Data frame with list-column `spd`.
#' @return Data frame with `var_name` and nested `data_to_fit` tables.
get_spd_for_modelling <- function(data_source_spd) {
  assertthat::assert_that(
    is.data.frame(data_source_spd),
    msg = "`data_source_spd` must be a data frame."
  )

  assertthat::assert_that(
    "spd" %in% names(data_source_spd),
    msg = "`data_source_spd` must contain `spd`."
  )

  assertthat::assert_that(
    all(purrr::map_lgl(data_source_spd$spd, is.data.frame)),
    msg = "`spd` entries must be data frames."
  )

  res_data <-
    data_source_spd %>%
    tidyr::unnest(
      spd
    ) %>%
    tidyr::pivot_longer(
      cols = -c(dataset_id, age),
      names_to = "var_name",
      values_to = "value"
    ) %>%
    tidyr::drop_na(value) %>%
    dplyr::mutate(
      value = round(value, 3)
    ) %>%
    tidyr::nest(data_to_fit = c(dataset_id, age, value))

  return(res_data)
}
