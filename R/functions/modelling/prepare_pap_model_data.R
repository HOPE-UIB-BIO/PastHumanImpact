#' @title Prepare PAP temporal data for general models
#' @description
#' Convert nested PAP outputs into long model input with region, climatezone,
#' dataset, age, variable, value, and stratum columns.
#' @param data_properties Data frame with `dataset_id` and nested `data_merge`.
#' @param data_meta Data frame with `dataset_id`, `region`, and `climatezone`.
#' @param pap_vars Character vector of PAP variables to include.
#' @param age_from Numeric lower age bound.
#' @param age_to Numeric upper age bound.
#' @param min_records Integer minimum number of records per
#' `region x climatezone x variable`.
#' @param exclude_regions Character vector of regions to exclude.
#' @return Tibble with one row per PAP observation.
#' @examples
#' \dontrun{
#' data_model <- prepare_pap_model_data(data_properties, data_meta)
#' }
prepare_pap_model_data <- function(
  data_properties,
  data_meta,
  pap_vars = c(
    "n0",
    "n1",
    "n2",
    "n1_minus_n2",
    "n2_divided_by_n1",
    "n1_divided_by_n0",
    "roc",
    "dcca_axis_1",
    "density_diversity",
    "density_turnover"
  ),
  age_from = 0,
  age_to = 8500,
  min_records = min_n_records_per_climate_zone,
  exclude_regions = "Africa"
) {
  assertthat::assert_that(
    is.data.frame(data_properties),
    msg = "`data_properties` must be a data frame."
  )
  assertthat::assert_that(
    is.data.frame(data_meta),
    msg = "`data_meta` must be a data frame."
  )
  assertthat::assert_that(
    all(c("dataset_id", "data_merge") %in% names(data_properties)),
    msg = "`data_properties` must contain `dataset_id` and `data_merge`."
  )
  assertthat::assert_that(
    all(c("dataset_id", "region", "climatezone") %in% names(data_meta)),
    msg = "`data_meta` must contain `dataset_id`, `region`, and `climatezone`."
  )
  assertthat::assert_that(
    is.character(pap_vars),
    length(pap_vars) > 0,
    msg = "`pap_vars` must be a non-empty character vector."
  )
  assertthat::assert_that(
    is.numeric(age_from),
    length(age_from) == 1,
    is.numeric(age_to),
    length(age_to) == 1,
    age_from <= age_to,
    msg = "`age_from` and `age_to` must be numeric bounds."
  )
  assertthat::assert_that(
    assertthat::is.count(min_records),
    msg = "`min_records` must be a positive integer."
  )

  data_long <-
    data_properties %>%
    dplyr::inner_join(
      data_meta %>%
        dplyr::select(dataset_id, region, climatezone),
      by = "dataset_id"
    ) %>%
    tidyr::unnest(data_merge) %>%
    dplyr::filter(
      !region %in% exclude_regions,
      age >= age_from,
      age <= age_to
    )

  assertthat::assert_that(
    all(c("age", pap_vars) %in% names(data_long)),
    msg = "`data_properties$data_merge` must contain `age` and all `pap_vars`."
  )

  data_valid_strata <-
    data_long %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(pap_vars),
      names_to = "variable",
      values_to = "value"
    ) %>%
    tidyr::drop_na(region, climatezone, value) %>%
    dplyr::group_by(region, climatezone, variable) %>%
    dplyr::summarise(
      n_records = dplyr::n_distinct(dataset_id),
      .groups = "drop"
    ) %>%
    dplyr::filter(n_records >= min_records)

  res_data <-
    data_long %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(pap_vars),
      names_to = "variable",
      values_to = "value"
    ) %>%
    tidyr::drop_na(region, climatezone, value) %>%
    dplyr::inner_join(
      data_valid_strata,
      by = c("region", "climatezone", "variable")
    ) %>%
    dplyr::mutate(
      age_ka = age / 1000,
      stratum = stringr::str_c(region, climatezone, sep = "__"),
      dataset_id = as.factor(dataset_id),
      stratum = as.factor(stratum)
    ) %>%
    dplyr::select(
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

  return(res_data)
}
