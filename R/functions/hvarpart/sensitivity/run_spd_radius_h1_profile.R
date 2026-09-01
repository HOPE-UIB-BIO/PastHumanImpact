#' @title Run one strict-radius H1 sensitivity profile
#' @description Preserve the established SPD-radius entry point while
#' delegating common temporal, spatial, and aggregation work to
#' `run_h1_control_profile()`.
#' @inheritParams run_h1_control_profile
#'
#' @return Named list of radius-tagged H1 inputs, results, and provenance.
#'
#' @export
run_spd_radius_h1_profile <- function(
  data_predictors_profile,
  data_properties_filtered,
  data_meta,
  response_vars,
  predictor_vars,
  analysis_config,
  data_profiles
) {
  run_h1_control_profile(
    data_predictors_profile = data_predictors_profile,
    data_properties_filtered = data_properties_filtered,
    data_meta = data_meta,
    response_vars = response_vars,
    predictor_vars = predictor_vars,
    analysis_config = analysis_config,
    data_profiles = data_profiles
  )
}
