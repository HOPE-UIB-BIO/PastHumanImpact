#' Run one cohort and proxy variant through reusable H1 controls
#'
#' @param data_predictors_cohort Filtered nested predictors for one cohort.
#' @param cohort Chronology cohort key.
#' @param proxy_variant Human-proxy variant key.
#' @inheritParams run_h1_control_profile
#'
#' @return Named list of tagged H1 results and provenance.
#'
#' @export
run_human_event_h1_profile <- function(
  data_predictors_cohort,
  cohort,
  proxy_variant,
  data_properties_filtered,
  data_meta,
  response_vars,
  analysis_config,
  data_profiles
) {
  allowed_cohorts <- c("as_coded", "observed_events_only")
  allowed_variants <- c("spd", "spd_events", "events")
  assertthat::assert_that(
    cohort %in% allowed_cohorts,
    proxy_variant %in% allowed_variants,
    msg = "Human-event H1 scenario identifiers are invalid."
  )

  configuration_reference <-
    switch(
      cohort,
      "as_coded" = "human_event_inclusion_as_coded",
      "observed_events_only" = "human_event_inclusion_observed_only"
    )
  radius_specification <-
    if (proxy_variant == "events") {
      "not_applicable"
    } else {
      "250_km_with_500_km_fallback"
    }
  data_predictors_profile <-
    data_predictors_cohort |>
    dplyr::mutate(
      radius_km = if (proxy_variant == "events") NA_real_ else 250,
      spd_radius_specification = radius_specification,
      .after = "dataset_id"
    )
  predictor_resolver <-
    function(region, available_columns) {
      resolve_region_event_predictor_specification(
        region = region,
        proxy_variant = proxy_variant,
        available_columns = available_columns
      )
    }
  predictor_columns <-
    c(
      "spd",
      "bi",
      "fi",
      "fc",
      "ec",
      "cc",
      "es",
      "no_impact",
      "weak",
      "medium",
      "strong",
      "ei",
      "temp_annual",
      "temp_cold",
      "prec_summer",
      "prec_win"
    )

  result <- run_h1_control_profile(
    data_predictors_profile = data_predictors_profile,
    data_properties_filtered = data_properties_filtered,
    data_meta = data_meta,
    response_vars = response_vars,
    predictor_vars = predictor_resolver,
    predictor_columns = predictor_columns,
    analysis_config = analysis_config,
    data_profiles = data_profiles,
    human_proxy = proxy_variant,
    configuration_reference = configuration_reference,
    tag_values = list(
      cohort = cohort,
      proxy_variant = proxy_variant
    )
  )

  result |>
    purrr::map(
      ~ add_human_event_predictor_provenance(
        data_source = .x,
        data_meta = data_meta,
        proxy_variant = proxy_variant
      )
    )
}
