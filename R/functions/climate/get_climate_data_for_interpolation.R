#' @title Prepare climate data for interpolation
#' @description
#' Unnest climate records, pivot to long format, and nest by selected
#' variables.
#' @param data_source Data frame with list-column `climate_data`.
#' @param sel_var Character vector of variable names to retain.
#' @return Data frame with columns `var_name` and nested `data_to_fit`.
get_climate_data_for_interpolation <- function(data_source,
                                               sel_var = c(
                                                 "temp_annual",
                                                 "temp_cold",
                                                 "prec_annual",
                                                 "prec_summer",
                                                 "prec_win"
                                               )) {
  assertthat::assert_that(
    is.data.frame(data_source),
    msg = "`data_source` must be a data frame."
  )

  assertthat::assert_that(
    "climate_data" %in% names(data_source),
    msg = "`data_source` must contain `climate_data`."
  )

  assertthat::assert_that(
    is.character(sel_var),
    msg = "`sel_var` must be a character vector."
  )

  assertthat::assert_that(
    all(purrr::map_lgl(data_source$climate_data, is.data.frame)),
    msg = "`climate_data` entries must be data frames."
  )

  res_data <-
    data_source %>%
    tidyr::unnest(climate_data) %>%
    tidyr::pivot_longer(
      temp_annual:gdm,
      names_to = "var_name",
      values_to = "value"
    ) %>%
    dplyr::select(-time_id) %>%
    tidyr::nest(data_to_fit = c(-var_name)) %>%
    dplyr::filter(
      var_name %in% sel_var
    )

  return(res_data)
}
