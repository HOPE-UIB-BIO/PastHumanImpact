#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#               Multidimensional-shift H2 models
#
#
#                   O. Mottl, V.A. Felde
#                         2024
#
#----------------------------------------------------------#
# Defines the multidimensional-shift h2 models target graph.
# Run with:
#   R/analyses/04_h2_multidimensional_shifts/00_run.R
# Sourcing this script only declares targets; it does not execute them.

#----------------------------------------------------------#
# 0. Configure pipeline -----
#----------------------------------------------------------#

library(here)

# Load configuration
source(
  here::here(
    "R/00_Config_file.R"
  )
)

# - Load meta data
source(
  here::here(
    "R/analyses/01_data_preparation/01_metadata/02_metadata.R"
  )
)

store_paps <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "data_preparation/paps"
  )

runner_data_preparation <-
  "R/analyses/01_data_preparation/00_run.R"

store_temporal_exports <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "temporal_models/exports"
  )

runner_temporal <-
  "R/analyses/03_temporal_models/00_run.R"

#----------------------------------------------------------#
# 1. Define targets -----
#----------------------------------------------------------#

list(
  # - path to data for multidimensional shifts ----
  # Why: Fingerprint PAPs so upstream changes invalidate this pipeline store.
  targets::tar_target(
    name = "fingerprint_paps",
    command = compute_target_store_fingerprint(
      store = store_paps,
      target_names = "data_m2_filtered",
      runner = runner_data_preparation
    ),
    cue = targets::tar_cue(mode = "always")
  ),
  # - load data for multidimensional shifts
  # Why: Prepare m2 filtered so downstream targets share one canonical dataset.
  targets::tar_target(
    name = "data_m2_filtered",
    command = {
      fingerprint_paps

      load_target_store_value(
        store = store_paps,
        target_name = "data_m2_filtered",
        runner = runner_data_preparation
      )
    }
  ),
  # Why: Fingerprint temporal exports so upstream changes invalidate this
  #   pipeline store.
  targets::tar_target(
    name = "fingerprint_temporal_exports",
    command = compute_target_store_fingerprint(
      store = store_temporal_exports,
      target_names = "data_temporal_model_predictions",
      runner = runner_temporal
    ),
    cue = targets::tar_cue(mode = "always")
  ),
  # - load all models
  # Why: Compute predicted merged once so downstream summaries reuse the same
  #   result.
  targets::tar_target(
    name = "mod_predicted_merged",
    command = {
      fingerprint_temporal_exports

      load_target_store_value(
        store = store_temporal_exports,
        target_name = "data_temporal_model_predictions",
        runner = runner_temporal
      )
    }
  ),
  # - merge datasets for hvar analyses of multidimensional shifts
  # Why: Prepare for hvar H2 so downstream targets share one canonical dataset.
  targets::tar_target(
    name = "data_for_hvar_h2",
    command = prepare_h2_hvarpart_data(
      data_m2 = data_m2_filtered,
      data_predictors = mod_predicted_merged
    )
  ),
  # - run hierarchical variation partitioning
  # Why: Compute hvar H2 SPD once so downstream summaries reuse the same result.
  targets::tar_target(
    name = "output_hvar_h2_spd",
    command = fit_hvarpart_models(
      data_source = data_for_hvar_h2,
      response_vars = NULL,
      response_dist = NULL,
      data_response_dist = "m2",
      predictor_vars = list(
        human = c("spd"),
        climate = c(
          "temp_annual",
          "temp_cold",
          "prec_summer",
          "prec_win"
        )
      ),
      run_all_predictors = FALSE,
      time_series = FALSE,
      get_significance = FALSE
    )
  ),
  # - preserve raw HVarPart components and diagnostics ----
  # Why: Prepare hvarpart H2 importance so downstream targets share one
  #   canonical dataset.
  targets::tar_target(
    name = "data_hvarpart_h2_importance",
    command = compute_hvarpart_importance(
      data_source = output_hvar_h2_spd |>
        dplyr::mutate(analysis = "h2_spd"),
      id_cols = c(
        "analysis",
        "region",
        "climatezone"
      )
    )
  ),
  # Why: Materialize hvarpart H2 audit overall so downstream reporting uses an
  #   auditable result.
  targets::tar_target(
    name = "table_hvarpart_h2_audit_overall",
    command = summarise_hvarpart_audit(
      data_importance = data_hvarpart_h2_importance,
      group_vars = "analysis"
    )
  ),
  # Why: Materialize hvarpart H2 profiles overall so downstream reporting uses
  #   an auditable result.
  targets::tar_target(
    name = "table_hvarpart_h2_profiles_overall",
    command = diagnose_hvarpart_importance_profiles(
      data_importance = data_hvarpart_h2_importance,
      group_vars = "analysis"
    )
  ),
  # Why: Materialize hvarpart H2 audit strata so downstream reporting uses an
  #   auditable result.
  targets::tar_target(
    name = "table_hvarpart_h2_audit_strata",
    command = summarise_hvarpart_audit(
      data_importance = data_hvarpart_h2_importance,
      group_vars = c(
        "analysis",
        "region",
        "climatezone"
      )
    )
  ),
  # Why: Materialize hvarpart H2 profiles strata so downstream reporting uses an
  #   auditable result.
  targets::tar_target(
    name = "table_hvarpart_h2_profiles_strata",
    command = diagnose_hvarpart_importance_profiles(
      data_importance = data_hvarpart_h2_importance,
      group_vars = c(
        "analysis",
        "region",
        "climatezone"
      )
    )
  )
)
