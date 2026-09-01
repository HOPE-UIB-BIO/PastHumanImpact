#' @title Validate analysis profiles
#' @description
#' Validate the declarative analysis-profile registry used by project
#' pipelines.
#' @param data_profiles Data frame containing analysis profiles.
#' @return Invisibly returns `data_profiles` when the contract is satisfied.
#' @examples
#' \dontrun{
#' validate_analysis_profiles(data_profiles = profiles)
#' }
validate_analysis_profiles <- function(data_profiles) {
  required_columns <-
    c(
      "profile_id",
      "analysis_family",
      "analytical_unit",
      "human_proxy",
      "human_predictor_specification",
      "spd_radius_specification",
      "structural_control",
      "profile_role",
      "enabled",
      "seed",
      "configuration_reference"
    )

  assertthat::assert_that(
    is.data.frame(data_profiles),
    all(required_columns %in% names(data_profiles)),
    msg = "Analysis profiles are missing required columns."
  )

  assertthat::assert_that(
    nrow(data_profiles) > 0L,
    !anyDuplicated(data_profiles[["profile_id"]]),
    all(!is.na(data_profiles[["profile_id"]])),
    all(nzchar(data_profiles[["profile_id"]])),
    msg = "Analysis profile IDs must be unique and non-empty."
  )

  allowed_roles <-
    c("canonical", "sensitivity")

  assertthat::assert_that(
    all(data_profiles[["profile_role"]] %in% allowed_roles),
    is.logical(data_profiles[["enabled"]]),
    all(!is.na(data_profiles[["enabled"]])),
    msg = "Analysis profile roles and enabled values are invalid."
  )

  profile_combination_keys <-
    paste(
      data_profiles[["analysis_family"]],
      data_profiles[["analytical_unit"]],
      data_profiles[["human_proxy"]],
      data_profiles[["structural_control"]],
      sep = "::"
    )

  supported_combination_keys <-
    c(
      "h1::within_dataset::spd::none",
      "h1::within_dataset::spd::time",
      "h1::within_dataset::events::none",
      "h1::within_dataset::events::time",
      "h1::time_slice::spd::none",
      "h1::time_slice::spd::space",
      "h1::time_slice::events::none",
      "h1::time_slice::events::space",
      "h1::spatial_aggregation::spd::time_and_space",
      "h1::within_dataset::spd_events::time",
      "h1::time_slice::spd_events::space",
      "h1::spatial_aggregation::spd_events::time_and_space",
      "h1::spatial_aggregation::events::time_and_space"
    )

  assertthat::assert_that(
    all(profile_combination_keys %in% supported_combination_keys),
    all(
      data_profiles[["human_proxy"]] != "events" |
        data_profiles[["spd_radius_specification"]] == "not_applicable"
    ),
    all(
      !data_profiles[["human_proxy"]] %in% c("spd", "spd_events") |
        data_profiles[["spd_radius_specification"]] != "not_applicable"
    ),
    msg = "Analysis profiles contain an unsupported combination."
  )

  canonical_scenarios <-
    data_profiles |>
    dplyr::filter(.data[["profile_role"]] == "canonical") |>
    dplyr::count(
      .data[["analysis_family"]],
      .data[["analytical_unit"]],
      .data[["human_proxy"]],
      .data[["structural_control"]]
    )

  assertthat::assert_that(
    all(canonical_scenarios[["n"]] == 1L),
    msg = paste(
      "Only one canonical profile is allowed for each scientific",
      "operation; parameter variants must be sensitivities."
    )
  )

  enabled_profiles <-
    data_profiles |>
    dplyr::filter(.data[["enabled"]])

  assertthat::assert_that(
    all(is.finite(enabled_profiles[["seed"]])),
    all(enabled_profiles[["seed"]] >= 0),
    all(nzchar(enabled_profiles[["configuration_reference"]])),
    msg = "Enabled profiles require seeds and configuration references."
  )

  return(invisible(data_profiles))
}
