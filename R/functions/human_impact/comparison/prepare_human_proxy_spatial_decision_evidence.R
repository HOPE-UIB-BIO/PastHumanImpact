#' @title Prepare spatial human-proxy decision evidence
#' @description Reconcile canonical, matched-SPD bridge, and filtered joint
#' models on one dataset cohort and derive spatial summaries and transitions.
#' @param canonical_balance Canonical square-root-SPD balance table.
#' @param canonical_components Canonical component table.
#' @param bridge_balance Matched-SPD bridge balance table.
#' @param joint_balance Filtered joint-human balance table.
#' @param filtered_temporal_unique Unique adjusted R-squared table for bridge
#' and joint time-controlled models.
#' @return A named list containing spatial common-cohort evidence.
#' @examples
#' \dontrun{
#' prepare_human_proxy_spatial_decision_evidence(a, b, c, d, e)
#' }
prepare_human_proxy_spatial_decision_evidence <- function(
  canonical_balance,
  canonical_components,
  bridge_balance,
  joint_balance,
  filtered_temporal_unique
) {
  inputs <- list(
    canonical_balance,
    canonical_components,
    bridge_balance,
    joint_balance,
    filtered_temporal_unique
  )
  assertthat::assert_that(
    all(purrr::map_lgl(inputs, is.data.frame)),
    msg = "Spatial decision-evidence inputs must be data frames."
  )
  common_ids <- canonical_balance |>
    dplyr::select(dplyr::all_of("dataset_id")) |>
    dplyr::inner_join(
      bridge_balance |> dplyr::select(dplyr::all_of("dataset_id")),
      by = "dataset_id"
    ) |>
    dplyr::inner_join(
      joint_balance |> dplyr::select(dplyr::all_of("dataset_id")),
      by = "dataset_id"
    ) |>
    dplyr::distinct()

  canonical_unique <- canonical_components |>
    dplyr::filter(
      .data[["measure"]] == "unique_adjusted_r2",
      .data[["component"]] %in% c("human", "climate", "time")
    ) |>
    dplyr::transmute(
      dataset_id = .data[["dataset_id"]],
      fraction = .data[["component"]],
      unique_adjusted_r_squared = .data[["value"]]
    ) |>
    tidyr::pivot_wider(
      names_from = "fraction",
      values_from = "unique_adjusted_r_squared",
      names_prefix = "unique_"
    )
  filtered_unique <- filtered_temporal_unique |>
    dplyr::filter(
      .data[["model_id"]] %in% c("spd_matched_bridge", "joint_filtered"),
      .data[["fraction"]] %in% c(
        "pure_human", "pure_climate", "pure_time"
      )
    ) |>
    dplyr::mutate(
      fraction = stringr::str_remove(.data[["fraction"]], "^pure_")
    ) |>
    dplyr::select(dplyr::all_of(c(
      "model_id", "dataset_id", "fraction", "adjusted_r_squared"
    ))) |>
    tidyr::pivot_wider(
      names_from = "fraction",
      values_from = "adjusted_r_squared",
      names_prefix = "unique_"
    )
  canonical_spatial <- prepare_human_proxy_spatial_model_balance(
    data_source = canonical_balance,
    common_ids = common_ids,
    model_id = "canonical_spd",
    model_label = "Canonical SPD"
  ) |>
    dplyr::left_join(canonical_unique, by = "dataset_id")
  bridge_spatial <- prepare_human_proxy_spatial_model_balance(
    data_source = bridge_balance,
    common_ids = common_ids,
    model_id = "spd_matched_bridge",
    model_label = "Matched SPD bridge"
  ) |>
    dplyr::left_join(
      filtered_unique |>
        dplyr::filter(.data[["model_id"]] == "spd_matched_bridge") |>
        dplyr::select(-dplyr::all_of("model_id")),
      by = "dataset_id"
    )
  joint_spatial <- prepare_human_proxy_spatial_model_balance(
    data_source = joint_balance,
    common_ids = common_ids,
    model_id = "joint_filtered",
    model_label = "Filtered joint human block"
  ) |>
    dplyr::left_join(
      filtered_unique |>
        dplyr::filter(.data[["model_id"]] == "joint_filtered") |>
        dplyr::select(-dplyr::all_of("model_id")),
      by = "dataset_id"
    )
  spatial_common <- dplyr::bind_rows(
    canonical_spatial,
    bridge_spatial,
    joint_spatial
  ) |>
    dplyr::left_join(
      joint_balance |>
        dplyr::semi_join(common_ids, by = "dataset_id") |>
        dplyr::transmute(
          dataset_id = .data[["dataset_id"]],
          continental_region = .data[["region"]],
          region = .data[["climatezone"]],
          long = .data[["long"]],
          lat = .data[["lat"]]
        ),
      by = "dataset_id"
    ) |>
    dplyr::mutate(
      dominance = dplyr::case_when(
        .data[["zero_balance"]] > 0 ~ "Human",
        .data[["zero_balance"]] < 0 ~ "Climate",
        TRUE ~ "Tie"
      )
    ) |>
    dplyr::arrange(.data[["model_id"]], .data[["dataset_id"]])
  spatial_summary <- spatial_common |>
    dplyr::summarise(
      n_sequences = dplyr::n(),
      human_dominant = sum(.data[["dominance"]] == "Human"),
      climate_dominant = sum(.data[["dominance"]] == "Climate"),
      ties = sum(.data[["dominance"]] == "Tie"),
      median_zero_balance = stats::median(.data[["zero_balance"]], na.rm = TRUE),
      median_total_adjusted_r_squared = stats::median(
        .data[["total_adjusted_r_squared"]], na.rm = TRUE
      ),
      median_unique_human = stats::median(
        .data[["unique_human"]], na.rm = TRUE
      ),
      median_unique_climate = stats::median(
        .data[["unique_climate"]], na.rm = TRUE
      ),
      median_unique_control = stats::median(
        .data[["unique_time"]], na.rm = TRUE
      ),
      .by = c("model_id", "model_label")
    )
  spatial_plot_records <- spatial_common |>
    dplyr::transmute(
      dataset_id = .data[["dataset_id"]],
      analysis = .data[["model_id"]],
      model_id = .data[["model_id"]],
      climatezone = .data[["region"]],
      region = .data[["continental_region"]],
      long = .data[["long"]],
      lat = .data[["lat"]],
      signed_difference = .data[["signed_difference"]],
      signed_weight = .data[["signed_weight"]],
      zero_balance = .data[["zero_balance"]],
      zero_weight = .data[["zero_weight"]]
    )
  spatial_dominance_wide <- spatial_common |>
    dplyr::select(dplyr::all_of(c(
      "dataset_id", "model_id", "dominance"
    ))) |>
    tidyr::pivot_wider(names_from = "model_id", values_from = "dominance")
  spatial_transitions <- dplyr::bind_rows(
    spatial_dominance_wide |>
      dplyr::count(
        from = .data[["canonical_spd"]],
        to = .data[["joint_filtered"]],
        name = "n_sequences"
      ) |>
      dplyr::mutate(contrast = "Canonical SPD to filtered joint"),
    spatial_dominance_wide |>
      dplyr::count(
        from = .data[["spd_matched_bridge"]],
        to = .data[["joint_filtered"]],
        name = "n_sequences"
      ) |>
      dplyr::mutate(contrast = "Matched SPD bridge to filtered joint")
  ) |>
    dplyr::select(dplyr::all_of("contrast"), dplyr::everything())
  res <- list(
    spatial_common_cohort = spatial_common,
    spatial_plot_records = spatial_plot_records,
    spatial_summary = spatial_summary,
    spatial_transitions = spatial_transitions
  )

  return(res)
}
