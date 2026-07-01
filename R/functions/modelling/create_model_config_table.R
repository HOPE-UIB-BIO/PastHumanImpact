#' @title Create a general model configuration table
#' @description
#' Build one lifecycle/configuration row per temporal model, including
#' predictor scaling, formula settings, and model eligibility diagnostics.
#' @param data_model Long model data containing model, predictor, response,
#' grouping, and stratum columns.
#' @param analysis Character scalar used when `data_model` has no `analysis`.
#' @param family_key Character scalar or named character vector of family keys.
#' @param engine Character scalar modelling engine.
#' @param model_profile Requested formula profile.
#' @param x_var Name of the original predictor column.
#' @param x_model_var Name of the standardised predictor column.
#' @param y_var Name of the response column.
#' @param group_var Name of the repeated-record grouping column.
#' @param stratum_var Name of the model stratum column.
#' @param smooth_basis Common smooth basis.
#' @param common_k Basis dimension for the common smooth.
#' @param group_k Basis dimension for dataset factor smooths.
#' @param age_min Numeric lower prediction age in years.
#' @param age_max Numeric upper prediction age in years.
#' @param timestep Numeric prediction timestep in years.
#' @param min_records Integer minimum records per stratum.
#' @param total_iterations Integer total MCMC iterations.
#' @param min_iterations_per_chain Integer minimum iterations per chain.
#' @param max_chains Integer maximum number of chains.
#' @param adapt_delta Numeric target average proposal acceptance probability.
#' @param max_treedepth Integer maximum NUTS tree depth.
#' @param large_model_min_records Integer record threshold for large models.
#' @param large_model_total_iterations Total iterations for large models.
#' @param large_model_adapt_delta `adapt_delta` for large models.
#' @param large_model_max_treedepth Maximum tree depth for large models.
#' @return Tibble with model definitions, lifecycle fields, and diagnostics.
#' @examples
#' \dontrun{
#' config <- create_model_config_table(data_model = data_pap_model)
#' }
create_model_config_table <- function(
  data_model,
  analysis = "pap_temporal",
  family_key = "student_identity",
  engine = "brms",
  model_profile = "within_stratum_dataset_fs",
  x_var = "age_ka",
  x_model_var = stringr::str_c(x_var, "scaled", sep = "_"),
  y_var = "value",
  group_var = "dataset_id",
  stratum_var = "stratum",
  smooth_basis = "cr",
  common_k = 8,
  group_k = 3,
  age_min = 0,
  age_max = 8500,
  timestep = 500,
  min_records = min_n_records_per_climate_zone,
  total_iterations = 3200,
  min_iterations_per_chain = 100,
  max_chains = 4,
  adapt_delta = 0.9,
  max_treedepth = 10,
  large_model_min_records = 100,
  large_model_total_iterations = 6400,
  large_model_adapt_delta = 0.95,
  large_model_max_treedepth = 12
) {
  assertthat::assert_that(
    is.data.frame(data_model),
    msg = "`data_model` must be a data frame."
  )

  required_data_cols <-
    c(
      group_var,
      stratum_var,
      "region",
      "climatezone",
      "variable",
      x_var,
      y_var
    )

  assertthat::assert_that(
    all(required_data_cols %in% names(data_model)),
    msg = "`data_model` is missing required model columns."
  )
  assertthat::assert_that(
    is.character(analysis),
    length(analysis) == 1,
    is.character(engine),
    length(engine) == 1,
    is.character(model_profile),
    length(model_profile) == 1,
    is.character(x_var),
    length(x_var) == 1,
    is.character(x_model_var),
    length(x_model_var) == 1,
    x_var != x_model_var,
    is.character(y_var),
    length(y_var) == 1,
    is.character(group_var),
    length(group_var) == 1,
    is.character(stratum_var),
    length(stratum_var) == 1,
    msg = "Model settings must be character scalars with distinct x names."
  )
  assertthat::assert_that(
    smooth_basis %in% c("cr", "tp"),
    msg = "`smooth_basis` must be `cr` or `tp`."
  )
  assertthat::assert_that(
    assertthat::is.count(common_k),
    assertthat::is.count(group_k),
    assertthat::is.count(total_iterations),
    assertthat::is.count(min_iterations_per_chain),
    assertthat::is.count(max_chains),
    assertthat::is.count(min_records),
    assertthat::is.count(max_treedepth),
    assertthat::is.count(large_model_min_records),
    assertthat::is.count(large_model_total_iterations),
    assertthat::is.count(large_model_max_treedepth),
    msg = "Iteration, basis, chain, and record settings must be integers."
  )
  assertthat::assert_that(
    is.numeric(adapt_delta),
    length(adapt_delta) == 1,
    adapt_delta > 0,
    adapt_delta < 1,
    is.numeric(large_model_adapt_delta),
    length(large_model_adapt_delta) == 1,
    large_model_adapt_delta > 0,
    large_model_adapt_delta < 1,
    msg = "`adapt_delta` values must be numeric scalars between 0 and 1."
  )

  data_work <-
    if (
      "analysis" %in% names(data_model)
    ) {
      data_model
    } else {
      data_model %>%
        dplyr::mutate(analysis = analysis)
    }

  model_group_cols <-
    c("analysis", "variable", "region", "climatezone")

  data_group_summary <-
    data_work %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c(model_group_cols, group_var)))
    ) %>%
    dplyr::summarise(
      group_response_n_unique = dplyr::n_distinct(.data[[y_var]]),
      .groups = "drop"
    ) %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(model_group_cols))
    ) %>%
    dplyr::summarise(
      datasets_with_response_variation = sum(
        group_response_n_unique > 1
      ),
      .groups = "drop"
    )

  data_summary <-
    data_work %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(model_group_cols))
    ) %>%
    dplyr::summarise(
      n_records = dplyr::n_distinct(.data[[group_var]]),
      n_strata = dplyr::n_distinct(.data[[stratum_var]]),
      response_n_unique = dplyr::n_distinct(.data[[y_var]]),
      predictor_n_unique = dplyr::n_distinct(.data[[x_var]]),
      x_mean = mean(.data[[x_var]]),
      x_sd = stats::sd(.data[[x_var]]),
      predictor_is_finite = all(is.finite(.data[[x_var]])),
      response_is_finite = all(is.finite(.data[[y_var]])),
      .groups = "drop"
    ) %>%
    dplyr::left_join(
      data_group_summary,
      by = model_group_cols
    ) %>%
    dplyr::mutate(
      output_id = stringr::str_c(
        analysis,
        variable,
        region,
        climatezone,
        sep = "__"
      ) %>%
        stringr::str_replace_all("[^A-Za-z0-9_]+", "_") %>%
        stringr::str_replace_all("^_|_$", ""),
      model_id = output_id
    )

  if (
    length(family_key) == 1 && is.null(names(family_key))
  ) {
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

    data_summary <-
      if (
        all(data_summary[["model_id"]] %in% names(family_key))
      ) {
        data_summary %>%
          dplyr::mutate(
            family_key = unname(family_key[model_id])
          )
      } else {
        data_summary %>%
          dplyr::mutate(
            family_key = unname(family_key[variable])
          )
      }
  }

  dataset_smooth_profiles <-
    c(
      "dataset_smooth",
      "stratum_fs_dataset_fs",
      "within_stratum_dataset_fs"
    )

  res_config <-
    data_summary %>%
    dplyr::mutate(
      engine = engine,
      requested_model_profile = model_profile,
      x_var = x_var,
      x_model_var = x_model_var,
      y_var = y_var,
      group_var = group_var,
      stratum_var = stratum_var,
      smooth_basis = smooth_basis,
      common_k = common_k,
      group_k = group_k,
      predictor_scaling_valid = predictor_is_finite &
        is.finite(x_mean) &
        is.finite(x_sd) &
        x_sd > 0,
      response_valid = response_is_finite & response_n_unique > 1,
      is_model_eligible = predictor_scaling_valid & response_valid,
      ineligibility_reason = dplyr::case_when(
        !predictor_is_finite ~ "non_finite_predictor",
        !is.finite(x_mean) | !is.finite(x_sd) | x_sd <= 0 ~
          "constant_or_invalid_predictor",
        !response_is_finite ~ "non_finite_response",
        response_n_unique <= 1 ~ "constant_response",
        .default = NA_character_
      ),
      use_profile_fallback = is_model_eligible &
        requested_model_profile %in% dataset_smooth_profiles &
        datasets_with_response_variation == 0,
      model_profile = dplyr::if_else(
        use_profile_fallback,
        "within_stratum_dataset_intercept",
        requested_model_profile
      ),
      profile_adjustment_reason = dplyr::if_else(
        use_profile_fallback,
        "no_within_dataset_response_variation",
        NA_character_
      ),
      age_min = age_min,
      age_max = age_max,
      timestep = timestep,
      min_records = min_records,
      total_iterations = dplyr::if_else(
        n_records >= large_model_min_records,
        large_model_total_iterations,
        total_iterations
      ),
      min_iterations_per_chain = min_iterations_per_chain,
      max_chains = max_chains,
      adapt_delta = dplyr::if_else(
        n_records >= large_model_min_records,
        large_model_adapt_delta,
        adapt_delta
      ),
      max_treedepth = dplyr::if_else(
        n_records >= large_model_min_records,
        large_model_max_treedepth,
        max_treedepth
      ),
      formula_text = purrr::pmap_chr(
        .l = list(
          x_model_var,
          y_var,
          group_var,
          smooth_basis,
          common_k,
          n_records,
          model_profile,
          stratum_var,
          group_k
        ),
        .f = ~ get_hgam_formula(
          x_var = ..1,
          y_var = ..2,
          group_var = ..3,
          smooth_basis = ..4,
          sel_k = ..5,
          n_groups = ..6,
          model_profile = ..7,
          stratum_var = ..8,
          group_k = ..9
        )
      ),
      last_run_date = NA_character_,
      last_run_start_time = NA_character_,
      last_run_end_time = NA_character_,
      last_run_time = NA_character_,
      last_run_rhat_test_pass = FALSE,
      last_run_rhat_test_value = NA_real_,
      last_run_rhat_q90 = NA_real_,
      last_run_rhat_max = NA_real_,
      last_run_neff_ratio_min = NA_real_,
      last_run_divergent_transitions = NA_integer_,
      last_run_max_treedepth_transitions = NA_integer_,
      last_run_loo_test_pass = FALSE,
      last_run_loo_test_value = NA_real_,
      need_to_run = is_model_eligible,
      need_to_be_evaluated = FALSE,
      last_evaluation_date = NA_character_,
      prediction_written = FALSE,
      last_prediction_date = NA_character_
    ) %>%
    dplyr::select(
      analysis,
      model_id,
      output_id,
      variable,
      region,
      climatezone,
      family_key,
      engine,
      requested_model_profile,
      model_profile,
      profile_adjustment_reason,
      is_model_eligible,
      ineligibility_reason,
      x_var,
      x_model_var,
      x_mean,
      x_sd,
      y_var,
      group_var,
      stratum_var,
      smooth_basis,
      common_k,
      group_k,
      response_n_unique,
      datasets_with_response_variation,
      predictor_n_unique,
      predictor_scaling_valid,
      response_valid,
      age_min,
      age_max,
      timestep,
      min_records,
      n_records,
      n_strata,
      formula_text,
      total_iterations,
      min_iterations_per_chain,
      max_chains,
      adapt_delta,
      max_treedepth,
      dplyr::everything()
    ) %>%
    dplyr::arrange(analysis, variable)

  return(res_config)
}
