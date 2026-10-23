#' @title Prepare temporal human-proxy decision evidence
#' @description Reconcile canonical, matched-SPD bridge, and filtered joint
#' temporal compositions on one continental-region-by-age cohort.
#' @param canonical_temporal Canonical square-root-SPD temporal composition.
#' @param bridge_temporal Matched-SPD bridge composition.
#' @param joint_temporal Filtered joint-human composition.
#' @param age_min Young boundary in cal yr BP.
#' @param age_max Old boundary in cal yr BP.
#' @return A named list containing temporal common-cohort evidence.
#' @examples
#' \dontrun{
#' prepare_human_proxy_temporal_decision_evidence(a, b, c)
#' }
prepare_human_proxy_temporal_decision_evidence <- function(
  canonical_temporal,
  bridge_temporal,
  joint_temporal,
  age_min = 2000,
  age_max = 8000
) {
  assertthat::assert_that(
    is.data.frame(canonical_temporal),
    is.data.frame(bridge_temporal),
    is.data.frame(joint_temporal),
    is.numeric(age_min), length(age_min) == 1L,
    is.numeric(age_max), length(age_max) == 1L,
    age_min < age_max,
    msg = "Temporal decision-evidence inputs do not satisfy the contract."
  )
  canonical_temporal_filtered <- canonical_temporal |>
    dplyr::filter(
      .data[["analysis"]] == "temporal_spd",
      dplyr::between(.data[["age"]], age_min, age_max)
    )
  common_units <- canonical_temporal_filtered |>
    dplyr::distinct(.data[["region"]], .data[["age"]]) |>
    dplyr::inner_join(
      bridge_temporal |>
        dplyr::distinct(.data[["region"]], .data[["age"]]),
      by = c("region", "age")
    ) |>
    dplyr::inner_join(
      joint_temporal |>
        dplyr::distinct(.data[["region"]], .data[["age"]]),
      by = c("region", "age")
    )
  temporal_common <- dplyr::bind_rows(
    canonical_temporal_filtered |>
      dplyr::semi_join(common_units, by = c("region", "age")) |>
      dplyr::transmute(
        model_id = "canonical_spd",
        model_label = "Canonical SPD",
        continental_region = .data[["region"]],
        age = .data[["age"]],
        predictor = .data[["predictor"]],
        allocation = .data[["allocation"]],
        unique_adjusted_r_squared = .data[["Unique"]],
        total_adjusted_r_squared = .data[["spatial_adjusted_r_squared"]]
      ),
    bridge_temporal |>
      dplyr::semi_join(common_units, by = c("region", "age")) |>
      dplyr::transmute(
        model_id = "spd_matched_bridge",
        model_label = "Matched SPD bridge",
        continental_region = .data[["region"]],
        age = .data[["age"]],
        predictor = .data[["predictor"]],
        allocation = .data[["allocation"]],
        unique_adjusted_r_squared = .data[["Unique"]],
        total_adjusted_r_squared = .data[["spatial_adjusted_r_squared"]]
      ),
    joint_temporal |>
      dplyr::semi_join(common_units, by = c("region", "age")) |>
      dplyr::transmute(
        model_id = "joint_filtered",
        model_label = "Filtered joint human block",
        continental_region = .data[["region"]],
        age = .data[["age"]],
        predictor = .data[["predictor"]],
        allocation = .data[["allocation"]],
        unique_adjusted_r_squared = .data[["Unique"]],
        total_adjusted_r_squared = .data[["spatial_adjusted_r_squared"]]
      )
  ) |>
    dplyr::arrange(
      .data[["model_id"]],
      .data[["continental_region"]],
      .data[["age"]],
      .data[["predictor"]]
    )
  temporal_wide <- temporal_common |>
    dplyr::select(dplyr::all_of(c(
      "model_id", "model_label", "continental_region", "age",
      "predictor", "allocation"
    ))) |>
    tidyr::pivot_wider(names_from = "predictor", values_from = "allocation") |>
    dplyr::mutate(
      dominance = dplyr::case_when(
        .data[["human"]] > .data[["climate"]] ~ "Human",
        .data[["human"]] < .data[["climate"]] ~ "Climate",
        TRUE ~ "Tie"
      )
    )
  temporal_summary <- temporal_wide |>
    dplyr::summarise(
      n_units = dplyr::n(),
      human_dominant = sum(.data[["dominance"]] == "Human"),
      climate_dominant = sum(.data[["dominance"]] == "Climate"),
      ties = sum(.data[["dominance"]] == "Tie"),
      median_human_allocation = stats::median(
        .data[["human"]], na.rm = TRUE
      ),
      median_climate_allocation = stats::median(
        .data[["climate"]], na.rm = TRUE
      ),
      median_space_allocation = stats::median(
        .data[["space"]], na.rm = TRUE
      ),
      .by = c("model_id", "model_label")
    )
  temporal_dominance_wide <- temporal_wide |>
    dplyr::select(dplyr::all_of(c(
      "continental_region", "age", "model_id", "dominance"
    ))) |>
    tidyr::pivot_wider(names_from = "model_id", values_from = "dominance")
  temporal_transitions <- dplyr::bind_rows(
    temporal_dominance_wide |>
      dplyr::count(
        from = .data[["canonical_spd"]],
        to = .data[["joint_filtered"]],
        name = "n_units"
      ) |>
      dplyr::mutate(contrast = "Canonical SPD to filtered joint"),
    temporal_dominance_wide |>
      dplyr::count(
        from = .data[["spd_matched_bridge"]],
        to = .data[["joint_filtered"]],
        name = "n_units"
      ) |>
      dplyr::mutate(contrast = "Matched SPD bridge to filtered joint")
  ) |>
    dplyr::select(dplyr::all_of("contrast"), dplyr::everything())
  res <- list(
    temporal_common_cohort = temporal_common,
    temporal_summary = temporal_summary,
    temporal_transitions = temporal_transitions
  )

  return(res)
}
