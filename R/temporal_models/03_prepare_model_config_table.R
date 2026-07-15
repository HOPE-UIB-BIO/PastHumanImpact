#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                  General temporal models
#                Prepare model config table
#
#                   O. Mottl, V.A. Felde
#                         2024
#
#----------------------------------------------------------#


#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

source(
  here::here(
    "R/00_Config_file.R"
  )
)

overwrite_table <- FALSE


#----------------------------------------------------------#
# 1. Load data and specs -----
#----------------------------------------------------------#

data_general_model <-
  RUtilpol::get_latest_file(
    file_name = "general_temporal_model_data",
    dir = paste0(
      data_storage_path,
      "Temporal_models/"
    )
  )

data_model_specs <-
  RUtilpol::get_latest_file(
    file_name = "general_temporal_model_specs",
    dir = paste0(
      data_storage_path,
      "Temporal_models/"
    )
  )


#----------------------------------------------------------#
# 2. Create config table -----
#----------------------------------------------------------#

family_key_by_model <-
  data_model_specs %>%
  dplyr::select(model_id, family_key) %>%
  tibble::deframe()

model_config_table <-
  create_model_config_table(
    data_model = data_general_model,
    analysis = "general_temporal",
    family_key = family_key_by_model,
    engine = "brms",
    model_profile = "within_stratum_dataset_fs",
    x_var = "age_ka",
    x_model_var = "age_ka_scaled",
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
    seed_base = set_seed,
    large_model_min_records = 100,
    large_model_total_iterations = 6400,
    large_model_adapt_delta = 0.95,
    large_model_max_treedepth = 12
  )


#----------------------------------------------------------#
# 3. Save table -----
#----------------------------------------------------------#

config_exists <-
  RUtilpol::get_latest_file_name(
    file_name = "general_model_config_table",
    dir = paste0(
      data_storage_path,
      "Temporal_models/"
    )
  ) %>%
  is.na() %>%
  isFALSE()

config_needs_refresh <- FALSE
config_current <- NULL

model_definition_cols <-
  c(
    "model_id",
    "variable",
    "region",
    "climatezone",
    "family_key",
    "engine",
    "requested_model_profile",
    "model_profile",
    "profile_adjustment_reason",
    "is_model_eligible",
    "ineligibility_reason",
    "x_var",
    "x_model_var",
    "x_mean",
    "x_sd",
    "y_var",
    "group_var",
    "stratum_var",
    "smooth_basis",
    "common_k",
    "group_k",
    "response_n_unique",
    "datasets_with_response_variation",
    "predictor_n_unique",
    "formula_text",
    "age_min",
    "age_max",
    "timestep",
    "min_records"
  )

if (
  isTRUE(config_exists)
) {
  config_current <-
    RUtilpol::get_latest_file(
      file_name = "general_model_config_table",
      dir = paste0(
        data_storage_path,
        "Temporal_models/"
      ),
      verbose = FALSE
    )

  seed_lifecycle_cols <-
    c(
      "seed_base",
      "seed_attempt",
      "sampling_seed",
      "seed_change_reason",
      "last_run_id",
      "last_run_seed_attempt",
      "last_run_seed"
    )

  config_seed_needs_refresh <-
    !all(seed_lifecycle_cols %in% names(config_current))

  if (
    isTRUE(config_seed_needs_refresh)
  ) {
    config_current <-
      config_current %>%
      dplyr::mutate(
        seed_base = as.integer(set_seed),
        seed_attempt = 1L,
        sampling_seed = get_model_seed(
          model_id = model_id,
          seed_attempt = seed_attempt,
          seed_base = seed_base
        ),
        seed_change_reason = "migrated_initial_model_seed",
        last_run_id = NA_character_,
        last_run_seed_attempt = NA_integer_,
        last_run_seed = NA_integer_
      )
  }

  if (
    !"adapt_delta" %in% names(config_current)
  ) {
    config_current <-
      config_current %>%
      dplyr::mutate(
        adapt_delta = 0.9
      )
  }

  if (
    !"max_treedepth" %in% names(config_current)
  ) {
    config_current <-
      config_current %>%
      dplyr::mutate(
        max_treedepth = 10
      )
  }

  if (
    !"last_run_rhat_q90" %in% names(config_current)
  ) {
    config_current <-
      config_current %>%
      dplyr::mutate(
        last_run_rhat_q90 = NA_real_
      )
  }

  if (
    !"last_run_rhat_max" %in% names(config_current)
  ) {
    config_current <-
      config_current %>%
      dplyr::mutate(
        last_run_rhat_max = NA_real_
      )
  }

  if (
    !"last_run_neff_ratio_min" %in% names(config_current)
  ) {
    config_current <-
      config_current %>%
      dplyr::mutate(
        last_run_neff_ratio_min = NA_real_
      )
  }

  if (
    !"last_run_divergent_transitions" %in% names(config_current)
  ) {
    config_current <-
      config_current %>%
      dplyr::mutate(
        last_run_divergent_transitions = NA_integer_
      )
  }

  if (
    !"last_run_max_treedepth_transitions" %in% names(config_current)
  ) {
    config_current <-
      config_current %>%
      dplyr::mutate(
        last_run_max_treedepth_transitions = NA_integer_
      )
  }

  config_provenance_needs_refresh <-
    !all(
      c(
        "model_file_name",
        "model_chain_seeds_json",
        "model_seed_source",
        "model_provenance_status",
        "model_audit_reason"
      ) %in% names(config_current)
    )

  if (
    !"model_file_name" %in% names(config_current)
  ) {
    config_current[["model_file_name"]] <- NA_character_
  }
  if (
    !"model_chain_seeds_json" %in% names(config_current)
  ) {
    config_current[["model_chain_seeds_json"]] <- NA_character_
  }
  if (
    !"model_seed_source" %in% names(config_current)
  ) {
    config_current[["model_seed_source"]] <- "not_audited"
  }
  if (
    !"model_provenance_status" %in% names(config_current)
  ) {
    config_current[["model_provenance_status"]] <- "not_audited"
  }
  if (
    !"model_audit_reason" %in% names(config_current)
  ) {
    config_current[["model_audit_reason"]] <- NA_character_
  }

  config_needs_refresh <-
    isTRUE(config_seed_needs_refresh) ||
    isTRUE(config_provenance_needs_refresh) ||
    !all(c("output_id", "region", "climatezone") %in% names(config_current)) ||
    !all(model_definition_cols %in% names(config_current)) ||
    !setequal(config_current[["model_id"]], model_config_table[["model_id"]])

  if (
    isFALSE(config_needs_refresh)
  ) {
    config_needs_refresh <-
      !isTRUE(
        all.equal(
          config_current %>%
            dplyr::select(dplyr::all_of(model_definition_cols)) %>%
            dplyr::arrange(model_id),
          model_config_table %>%
            dplyr::select(dplyr::all_of(model_definition_cols)) %>%
            dplyr::arrange(model_id),
          check.attributes = FALSE,
          tolerance = sqrt(.Machine$double.eps)
        )
      )
  }
}

if (
  isTRUE(overwrite_table) ||
    isFALSE(config_exists) ||
    isTRUE(config_needs_refresh)
) {
  if (
    isTRUE(config_exists) &&
      all(model_definition_cols %in% names(config_current)) &&
      all(model_definition_cols %in% names(model_config_table))
  ) {
    model_definition_current <-
      config_current %>%
      dplyr::select(dplyr::all_of(model_definition_cols)) %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::where(is.numeric),
          ~ signif(.x, digits = 15)
        )
      ) %>%
      dplyr::mutate(
        model_definition = do.call(
          paste,
          c(
            dplyr::across(dplyr::everything()),
            sep = "\r"
          )
        )
      ) %>%
      dplyr::select(model_id, model_definition_current = model_definition)

    model_definition_new <-
      model_config_table %>%
      dplyr::select(dplyr::all_of(model_definition_cols)) %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::where(is.numeric),
          ~ signif(.x, digits = 15)
        )
      ) %>%
      dplyr::mutate(
        model_definition = do.call(
          paste,
          c(
            dplyr::across(dplyr::everything()),
            sep = "\r"
          )
        )
      ) %>%
      dplyr::select(model_id, model_definition_new = model_definition)

    model_ids_changed <-
      model_definition_new %>%
      dplyr::left_join(
        model_definition_current,
        by = "model_id"
      ) %>%
      dplyr::filter(
        is.na(model_definition_current) |
          model_definition_new != model_definition_current
      ) %>%
      dplyr::pull(model_id)

    lifecycle_cols <-
      c(
        "total_iterations",
        "min_iterations_per_chain",
        "max_chains",
        "adapt_delta",
        "max_treedepth",
        "seed_base",
        "seed_attempt",
        "sampling_seed",
        "seed_change_reason",
        "last_run_date",
        "last_run_id",
        "last_run_seed_attempt",
        "last_run_seed",
        "model_file_name",
        "model_chain_seeds_json",
        "model_seed_source",
        "model_provenance_status",
        "model_audit_reason",
        "last_run_start_time",
        "last_run_end_time",
        "last_run_time",
        "last_run_rhat_test_pass",
        "last_run_rhat_test_value",
        "last_run_rhat_q90",
        "last_run_rhat_max",
        "last_run_neff_ratio_min",
        "last_run_divergent_transitions",
        "last_run_max_treedepth_transitions",
        "last_run_loo_test_pass",
        "last_run_loo_test_value",
        "need_to_run",
        "need_to_be_evaluated",
        "last_evaluation_date",
        "prediction_written",
        "last_prediction_date"
      )

    lifecycle_cols <-
      lifecycle_cols[
        lifecycle_cols %in% names(config_current) &
          lifecycle_cols %in% names(model_config_table)
      ]

    config_lifecycle_current <-
      config_current %>%
      dplyr::select(model_id, dplyr::all_of(lifecycle_cols)) %>%
      dplyr::rename_with(
        .fn = ~ paste0(.x, "_current"),
        .cols = -model_id
      )

    model_config_table <-
      model_config_table %>%
      dplyr::left_join(
        config_lifecycle_current,
        by = "model_id"
      )

    for (
      lifecycle_col in lifecycle_cols
    ) {
      lifecycle_col_current <-
        paste0(
          lifecycle_col,
          "_current"
        )

      use_current_value <-
        !(model_config_table[["model_id"]] %in% model_ids_changed) &
          !is.na(model_config_table[[lifecycle_col_current]])

      model_config_table[[lifecycle_col]][use_current_value] <-
        model_config_table[[lifecycle_col_current]][use_current_value]
    }

    model_config_table <-
      model_config_table %>%
      dplyr::select(
        -dplyr::ends_with("_current")
      )
  }

  RUtilpol::save_latest_file(
    object_to_save = model_config_table,
    file_name = "general_model_config_table",
    dir = paste0(
      data_storage_path,
      "Temporal_models/"
    ),
    prefered_format = "csv"
  )
}
