#' Assemble source tables for human-event H1 comparisons
#'
#' @param data_time_status Within-dataset fit status table.
#' @param data_time_balance Within-dataset contribution and balance table.
#' @param data_time_unique Within-dataset unique adjusted-R-squared table.
#' @param data_spatial_status Continent-age fit status table.
#' @param data_spatial_rankings Continent-age contribution/ranking table.
#' @param data_spatial_composition Continent-age zero-allocation table.
#' @param data_spatial_unique Continent-age unique adjusted-R-squared table.
#' @param data_meta Dataset metadata with region and climate zone.
#'
#' @return A list with `dataset` and `region_age` all-available tables.
#'
#' @export
build_human_event_evidence_tables <- function(
  data_time_status,
  data_time_balance,
  data_time_unique,
  data_spatial_status,
  data_spatial_rankings,
  data_spatial_composition,
  data_spatial_unique,
  data_meta
) {
  scenario_dataset_keys <- c("cohort", "proxy_variant", "dataset_id")
  scenario_age_keys <- c("cohort", "proxy_variant", "region", "age")

  input_tables <- list(
    data_time_status,
    data_time_balance,
    data_time_unique,
    data_spatial_status,
    data_spatial_rankings,
    data_spatial_composition,
    data_spatial_unique,
    data_meta
  )
  assertthat::assert_that(
    all(purrr::map_lgl(input_tables, is.data.frame)),
    msg = "Human-event evidence inputs do not satisfy the contract."
  )

  time_unique <-
    data_time_unique |>
    dplyr::select(
      dplyr::all_of(c(
        scenario_dataset_keys,
        "fraction",
        "adjusted_r_squared"
      ))
    ) |>
    tidyr::pivot_wider(
      names_from = "fraction",
      values_from = "adjusted_r_squared",
      names_prefix = "unique_adjusted_r2_"
    )
  dataset <-
    data_time_status |>
    dplyr::select(dplyr::all_of(c(scenario_dataset_keys, "status"))) |>
    dplyr::left_join(
      data_time_balance |>
        dplyr::select(dplyr::all_of(c(
          scenario_dataset_keys,
          "total_adjusted_r_squared",
          "human",
          "climate",
          "time",
          "signed_difference",
          "signed_balance",
          "zero_balance",
          "signed_ranking",
          "zero_ranking",
          "requested_regional_events",
          "retained_human_predictors",
          "reference_category"
        ))),
      by = scenario_dataset_keys,
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(
      time_unique,
      by = scenario_dataset_keys,
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(
      data_meta |>
        dplyr::select(
          dplyr::all_of(c("dataset_id", "region", "climatezone"))
        ) |>
        dplyr::distinct(),
      by = "dataset_id",
      relationship = "many-to-one"
    )

  spatial_allocations <-
    data_spatial_composition |>
    dplyr::select(
      dplyr::all_of(c(
        scenario_age_keys,
        "predictor",
        "allocation"
      ))
    ) |>
    tidyr::pivot_wider(
      names_from = "predictor",
      values_from = "allocation",
      names_prefix = "zero_allocation_"
    )
  spatial_unique <-
    data_spatial_unique |>
    dplyr::select(
      dplyr::all_of(c(
        scenario_age_keys,
        "fraction",
        "adjusted_r_squared"
      ))
    ) |>
    tidyr::pivot_wider(
      names_from = "fraction",
      values_from = "adjusted_r_squared",
      names_prefix = "unique_adjusted_r2_"
    )
  region_age <-
    data_spatial_status |>
    dplyr::select(
      dplyr::all_of(c(scenario_age_keys, "status", "selection_status"))
    ) |>
    dplyr::left_join(
      data_spatial_rankings |>
        dplyr::select(dplyr::all_of(c(
          scenario_age_keys,
          "human_climate_only_balance",
          "controlled_human",
          "controlled_climate",
          "controlled_balance",
          "human_climate_only_ranking",
          "controlled_ranking",
          "ranking_changed",
          "requested_regional_events",
          "retained_human_predictors",
          "reference_category"
        ))),
      by = scenario_age_keys,
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(
      spatial_allocations,
      by = scenario_age_keys,
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(
      spatial_unique,
      by = scenario_age_keys,
      relationship = "one-to-one"
    )

  dataset <-
    dataset |>
    dplyr::select(
      -dplyr::any_of(c(
        "requested_regional_events",
        "retained_human_predictors",
        "reference_category"
      ))
    ) |>
    dplyr::group_split(.data[["proxy_variant"]], .keep = TRUE) |>
    purrr::map_dfr(
      ~ add_human_event_predictor_provenance(
        data_source = .x,
        data_meta = data_meta,
        proxy_variant = unique(.x[["proxy_variant"]])
      )
    )
  region_age <-
    region_age |>
    dplyr::select(
      -dplyr::any_of(c(
        "requested_regional_events",
        "retained_human_predictors",
        "reference_category"
      ))
    ) |>
    dplyr::group_split(.data[["proxy_variant"]], .keep = TRUE) |>
    purrr::map_dfr(
      ~ add_human_event_predictor_provenance(
        data_source = .x,
        data_meta = data_meta,
        proxy_variant = unique(.x[["proxy_variant"]])
      )
    )

  list(dataset = dataset, region_age = region_age)
}
