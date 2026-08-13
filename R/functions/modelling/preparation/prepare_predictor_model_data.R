#' @title Prepare predictor and event temporal model data
#' @description
#' Convert nested predictor outputs into long temporal model input for climate,
#' SPD, and event variables.
#' @param data_predictors Nested predictor data from the predictor target
#' pipeline.
#' @param data_meta Metadata used by `prepare_filtered_hvarpart_data()`.
#' @param data_regions Data frame with `dataset_id` and `region`.
#' @param data_climatezones Data frame with `dataset_id` and `climatezone`.
#' @param age_from Numeric lower age bound.
#' @param age_to Numeric upper age bound.
#' @param min_records Integer minimum records per region-climatezone-variable.
#' @param remove_private Logical passed to `prepare_filtered_hvarpart_data()`.
#' @return A list with `data_model` and `data_constant` tibbles.
#' @examples
#' \dontrun{
#' data_prepared <- prepare_predictor_model_data(
#'   data_predictors,
#'   data_meta,
#'   regions,
#'   climate_zones
#' )
#' }
prepare_predictor_model_data <- function(
  data_predictors,
  data_meta,
  data_regions,
  data_climatezones,
  age_from = 0,
  age_to = 8500,
  min_records = min_n_records_per_climate_zone,
  remove_private = TRUE
) {
  assertthat::assert_that(
    is.data.frame(data_predictors),
    msg = "`data_predictors` must be a data frame."
  )
  assertthat::assert_that(
    is.data.frame(data_meta),
    msg = "`data_meta` must be a data frame."
  )
  assertthat::assert_that(
    all(c("dataset_id", "region") %in% names(data_regions)),
    msg = "`data_regions` must contain `dataset_id` and `region`."
  )
  assertthat::assert_that(
    all(c("dataset_id", "climatezone") %in% names(data_climatezones)),
    msg = "`data_climatezones` must contain `dataset_id` and `climatezone`."
  )
  assertthat::assert_that(
    assertthat::is.count(min_records),
    msg = "`min_records` must be a positive integer."
  )

  data_pre_filter <-
    prepare_filtered_hvarpart_data(
      data_source = data_predictors,
      data_meta = data_meta,
      age_from = age_from,
      age_to = age_to,
      remove_private = remove_private
    )

  data_merge <-
    dplyr::inner_join(
      data_regions,
      data_climatezones,
      by = "dataset_id"
    ) %>%
    dplyr::filter(
      region != "Africa"
    ) %>%
    dplyr::inner_join(
      data_pre_filter,
      by = "dataset_id"
    ) %>%
    tidyr::unnest(data_merge) %>%
    tidyr::pivot_longer(
      cols = -c(dataset_id, region, climatezone, age),
      values_to = "value",
      names_to = "variable"
    ) %>%
    tidyr::drop_na(value)

  data_valid_variables <-
    data_merge %>%
    dplyr::filter(
      variable %in% c(
        "temp_annual",
        "temp_cold",
        "prec_summer",
        "prec_win",
        "bi",
        "fi",
        "fc",
        "ec",
        "ei",
        "cc",
        "es",
        "weak",
        "medium",
        "strong",
        "spd"
      )
    ) %>%
    dplyr::mutate(
      invalid_variable = dplyr::case_when(
        .default = FALSE,
        region == "Asia" & variable %in% c(
          "weak", "medium", "strong", "ec", "cc", "es"
        ) ~ TRUE,
        region == "Europe" & variable %in% c(
          "weak", "medium", "strong", "ei", "es"
        ) ~ TRUE,
        region == "North America" & variable %in% c(
          "weak", "medium", "strong", "fi", "ec", "ei", "cc"
        ) ~ TRUE,
        region == "Latin America" & variable %in% c(
          "medium", "fi", "fc", "ec", "ei", "cc", "es"
        ) ~ TRUE,
        region == "Oceania" & variable %in% c(
          "fi", "fc", "ec", "ei", "cc", "es"
        ) ~ TRUE
      )
    ) %>%
    dplyr::filter(!invalid_variable) %>%
    dplyr::select(-invalid_variable)

  data_filter_by_age <-
    data_valid_variables %>%
    dplyr::mutate(
      is_spd = dplyr::case_when(
        .default = FALSE,
        variable == "spd" ~ TRUE
      ),
      is_valid_age = dplyr::case_when(
        .default = TRUE,
        is_spd == TRUE & age < 2000 ~ FALSE
      )
    ) %>%
    dplyr::filter(
      is_valid_age == TRUE
    ) %>%
    dplyr::select(-c(is_spd, is_valid_age))

  valid_climate_zones_by_n_records <-
    data_filter_by_age %>%
    dplyr::group_by(region, climatezone, variable) %>%
    dplyr::distinct(dataset_id) %>%
    dplyr::summarise(
      .groups = "drop",
      n_records = dplyr::n()
    ) %>%
    dplyr::filter(
      n_records >= min_records
    )

  data_filter_by_climatezone <-
    data_filter_by_age %>%
    dplyr::inner_join(
      valid_climate_zones_by_n_records,
      by = c("region", "climatezone", "variable")
    )

  data_constant_variables <-
    data_filter_by_climatezone %>%
    dplyr::distinct(region, climatezone, variable) %>%
    dplyr::mutate(
      is_it_constant = purrr::pmap_lgl(
        .l = list(region, climatezone, variable),
        .f = ~ {
          data_filter_by_climatezone %>%
            dplyr::filter(
              region == ..1 &
                climatezone == ..2 &
                variable == ..3
            ) %>%
            dplyr::distinct(value) %>%
            nrow() == 1
        }
      )
    ) %>%
    dplyr::filter(is_it_constant) %>%
    dplyr::select(-is_it_constant)

  data_constant <-
    data_filter_by_climatezone %>%
    dplyr::inner_join(
      data_constant_variables,
      by = c("region", "climatezone", "variable")
    ) %>%
    dplyr::distinct(
      region,
      climatezone,
      variable,
      age,
      value
    )

  data_model <-
    data_filter_by_climatezone %>%
    dplyr::mutate(
      age_ka = age / 1000,
      stratum = stringr::str_c(region, climatezone, sep = "__"),
      analysis = dplyr::case_when(
        variable %in% c(
          "temp_annual",
          "temp_cold",
          "prec_summer",
          "prec_win",
          "spd"
        ) ~ "predictor_temporal",
        .default = "event_temporal"
      ),
      dataset_id = as.factor(dataset_id),
      stratum = as.factor(stratum)
    ) %>%
    dplyr::select(
      analysis,
      region,
      climatezone,
      stratum,
      dataset_id,
      age,
      age_ka,
      variable,
      value,
      n_records
    )

  res_data <-
    list(
      data_model = data_model,
      data_constant = data_constant
    )

  return(res_data)
}
