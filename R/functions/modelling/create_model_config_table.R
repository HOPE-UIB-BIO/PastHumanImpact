#' @title Create a general model configuration table
#' @description
#' Build one lifecycle/configuration row per model from long model input data.
#' @param data_model Data frame containing `dataset_id`, `stratum`, and
#' `variable`. If `analysis` is present, models are grouped by both `analysis`
#' and `variable`.
#' @param analysis Character scalar analysis name.
#' @param family_key Character scalar or named character vector of family keys.
#' @param engine Character scalar modelling engine.
#' @param model_profile Character scalar formula profile.
#' @param age_min Numeric lower prediction/fitting age.
#' @param age_max Numeric upper prediction/fitting age.
#' @param timestep Numeric prediction timestep.
#' @param min_records Integer minimum records per stratum.
#' @param total_iterations Integer total MCMC iterations.
#' @param min_iterations_per_chain Integer minimum iterations per chain.
#' @param max_chains Integer maximum number of chains.
#' @return Tibble with general model lifecycle fields.
#' @examples
#' \dontrun{
#' config <- create_model_config_table(data_model = data_pap_model)
#' }
create_model_config_table <- function(
  data_model,
  analysis = "pap_temporal",
  family_key = "student_identity",
  engine = "brms",
  model_profile = "stratum_fs",
  age_min = 0,
  age_max = 8500,
  timestep = 500,
  min_records = min_n_records_per_climate_zone,
  total_iterations = 3200,
  min_iterations_per_chain = 100,
  max_chains = 4
) {
  assertthat::assert_that(
    is.data.frame(data_model),
    msg = "`data_model` must be a data frame."
  )
  assertthat::assert_that(
    all(c("dataset_id", "stratum", "variable") %in% names(data_model)),
    msg = "`data_model` must contain `dataset_id`, `stratum`, and `variable`."
  )
  assertthat::assert_that(
    is.character(analysis),
    length(analysis) == 1,
    is.character(engine),
    length(engine) == 1,
    is.character(model_profile),
    length(model_profile) == 1,
    msg = "`analysis`, `engine`, and `model_profile` must be character scalars."
  )
  assertthat::assert_that(
    assertthat::is.count(total_iterations),
    assertthat::is.count(min_iterations_per_chain),
    assertthat::is.count(max_chains),
    assertthat::is.count(min_records),
    msg = "Iteration, chain, and record settings must be positive integers."
  )

  if (
    !"analysis" %in% names(data_model)
  ) {
    data_work <-
      data_model %>%
      dplyr::mutate(analysis = analysis)
  } else {
    data_work <-
      data_model
  }

  data_summary <-
    data_work %>%
    dplyr::group_by(analysis, variable) %>%
    dplyr::summarise(
      n_records = dplyr::n_distinct(dataset_id),
      n_strata = dplyr::n_distinct(stratum),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      model_id = stringr::str_c(analysis, variable, sep = "__")
    )

  if (length(family_key) == 1 && is.null(names(family_key))) {
    data_summary <-
      data_summary %>%
      dplyr::mutate(family_key = family_key)
  } else {
    assertthat::assert_that(
      !is.null(names(family_key)),
      all(data_summary[["variable"]] %in% names(family_key)) ||
        all(data_summary[["model_id"]] %in% names(family_key)),
      msg = paste(
        "Named `family_key` vectors must cover every model variable",
        "or every model ID."
      )
    )

    if (
      all(data_summary[["model_id"]] %in% names(family_key))
    ) {
      data_summary <-
        data_summary %>%
        dplyr::mutate(
          family_key = unname(family_key[model_id])
        )
    } else {
      data_summary <-
        data_summary %>%
        dplyr::mutate(
          family_key = unname(family_key[variable])
        )
    }
  }

  res_config <-
    data_summary %>%
    dplyr::mutate(
      engine = engine,
      model_profile = model_profile,
      x_var = "age_ka",
      y_var = "value",
      group_var = "dataset_id",
      stratum_var = "stratum",
      region = "all",
      climatezone = "all",
      age_min = age_min,
      age_max = age_max,
      timestep = timestep,
      min_records = min_records,
      total_iterations = total_iterations,
      min_iterations_per_chain = min_iterations_per_chain,
      max_chains = max_chains,
      last_run_date = NA_character_,
      last_run_start_time = NA_character_,
      last_run_end_time = NA_character_,
      last_run_time = NA_character_,
      last_run_rhat_test_pass = FALSE,
      last_run_rhat_test_value = NA_real_,
      last_run_loo_test_pass = FALSE,
      last_run_loo_test_value = NA_real_,
      need_to_run = TRUE,
      need_to_be_evaluated = FALSE,
      last_evaluation_date = NA_character_,
      prediction_written = FALSE,
      last_prediction_date = NA_character_
    ) %>%
    dplyr::select(
      analysis,
      model_id,
      variable,
      region,
      climatezone,
      family_key,
      engine,
      model_profile,
      x_var,
      y_var,
      group_var,
      stratum_var,
      age_min,
      age_max,
      timestep,
      min_records,
      n_records,
      n_strata,
      total_iterations,
      min_iterations_per_chain,
      max_chains,
      dplyr::everything()
    ) %>%
    dplyr::arrange(analysis, variable)

  return(res_config)
}
