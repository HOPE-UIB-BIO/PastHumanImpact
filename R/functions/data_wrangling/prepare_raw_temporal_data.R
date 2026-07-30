#' @title Prepare raw temporal figure data
#' @description
#' Combine pre-interpolation PAP and predictor pipeline outputs into one long
#' table for core-level temporal figures.
#' @param data_diversity Raw diversity and DCCA target with `var_name` and
#' `data_to_fit`.
#' @param data_roc Raw rate-of-change target with `var_name` and `data_to_fit`.
#' @param data_climate Raw climate target with `var_name` and `data_to_fit`.
#' @param data_spd Raw SPD target with `var_name` and `data_to_fit`.
#' @param dataset_ids Optional character vector of datasets to retain.
#' @param age_min Numeric minimum age in calibrated years BP.
#' @param age_max Numeric maximum age in calibrated years BP.
#' @param spd_age_min Numeric minimum valid SPD age in calibrated years BP.
#' @return
#' Tibble with `analysis`, `dataset_id`, `age`, `variable`, and `value`.
#' @details
#' Density PAPs are not returned because they are calculated directly on the
#' common age grid and have no separate pre-interpolation series.
#' @examples
#' \dontrun{
#' data_raw <- prepare_raw_temporal_data(
#'   data_diversity,
#'   data_roc,
#'   data_climate,
#'   data_spd,
#'   dataset_ids = "40579"
#' )
#' }
prepare_raw_temporal_data <- function(
  data_diversity,
  data_roc,
  data_climate,
  data_spd,
  dataset_ids = NULL,
  age_min = 500,
  age_max = 8500,
  spd_age_min = 2000
) {
  list_sources <-
    list(data_diversity, data_roc, data_climate, data_spd)

  assertthat::assert_that(
    all(purrr::map_lgl(list_sources, is.data.frame)),
    all(
      purrr::map_lgl(
        list_sources,
        ~ all(c("var_name", "data_to_fit") %in% names(.x))
      )
    ),
    msg = "Raw temporal sources must contain `var_name` and `data_to_fit`."
  )
  assertthat::assert_that(
    is.null(dataset_ids) || is.character(dataset_ids),
    is.numeric(age_min),
    length(age_min) == 1L,
    is.finite(age_min),
    is.numeric(age_max),
    length(age_max) == 1L,
    is.finite(age_max),
    age_min <= age_max,
    is.numeric(spd_age_min),
    length(spd_age_min) == 1L,
    is.finite(spd_age_min),
    msg = "Dataset and age filters must be valid."
  )

  vec_pap_variables <-
    c(
      "n0",
      "n1",
      "n2",
      "n1_minus_n2",
      "n2_divided_by_n1",
      "n1_divided_by_n0",
      "dcca_axis_1"
    )
  vec_climate_variables <-
    c("temp_annual", "temp_cold", "prec_summer", "prec_win")

  data_pap_raw <-
    dplyr::bind_rows(data_diversity, data_roc) %>%
    dplyr::filter(var_name %in% c(vec_pap_variables, "roc")) %>%
    dplyr::mutate(analysis = "pap_temporal")
  data_predictor_raw <-
    data_climate %>%
    dplyr::filter(var_name %in% vec_climate_variables) %>%
    dplyr::mutate(analysis = "predictor_temporal")
  data_spd_raw <-
    data_spd %>%
    dplyr::mutate(
      var_name = "spd",
      analysis = "predictor_temporal"
    )

  res_data <-
    dplyr::bind_rows(
      data_pap_raw,
      data_predictor_raw,
      data_spd_raw
    ) %>%
    dplyr::mutate(
      data_to_fit = purrr::map(
        data_to_fit,
        ~ .x %>%
          dplyr::filter(
            age >= age_min,
            age <= age_max,
            is.null(dataset_ids) |
              as.character(dataset_id) %in% dataset_ids
          ) %>%
          dplyr::select(dataset_id, age, value)
      )
    ) %>%
    tidyr::unnest(data_to_fit) %>%
    dplyr::transmute(
      analysis,
      dataset_id = as.character(dataset_id),
      age,
      variable = var_name,
      value
    ) %>%
    dplyr::filter(
      .data[["variable"]] != "spd" |
        .data[["age"]] >= spd_age_min
    ) %>%
    tidyr::drop_na(dataset_id, age, value)

  return(res_data)
}
