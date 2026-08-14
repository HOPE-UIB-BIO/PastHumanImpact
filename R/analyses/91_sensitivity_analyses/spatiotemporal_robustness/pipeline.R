#----------------------------------------------------------#
# H1 spatiotemporal balance robustness analysis
#----------------------------------------------------------#

library(here)

source(here::here("R/00_Config_file.R"))

store_inputs <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "analyses_h1/inputs"
  )

store_time_control <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "analyses_h1/time_control/spd"
  )

store_aggregation <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = paste(
      "analyses_h1/spatial_aggregation",
      "spd_human_climate_balance",
      sep = "/"
    )
  )

runner_h1 <-
  "R/analyses/02_h1_spatiotemporal_hvarpart/00_run.R"

list(
  targets::tar_target(
    name = data_time_controlled_balance_records,
    command = load_target_store_value(
      store = store_time_control,
      target_name = "data_time_controlled_balance_records",
      runner = runner_h1
    )
  ),
  targets::tar_target(
    name = table_spatiotemporal_balance_estimates,
    command = load_target_store_value(
      store = store_aggregation,
      target_name = "table_spatiotemporal_balance_estimates",
      runner = runner_h1
    )
  ),
  targets::tar_target(
    name = h1_analysis_config,
    command = load_target_store_value(
      store = store_inputs,
      target_name = "h1_analysis_config",
      runner = runner_h1
    )
  ),
  targets::tar_target(
    name = data_human_climate_only_matched_records,
    command = prepare_human_climate_only_records(
      data_records = data_time_controlled_balance_records
    )
  ),
  targets::tar_target(
    name = table_human_climate_only_matched_estimates,
    command = summarise_spatial_importance_subset(
      data_subset = data_human_climate_only_matched_records,
      sensitivity_type = "human_climate_only"
    ) |>
      dplyr::mutate(
        ranking = dplyr::case_when(
          .data[["importance_balance"]] > 0 ~ "human",
          .data[["importance_balance"]] < 0 ~ "climate",
          .default = "tie"
        )
      )
  ),
  targets::tar_target(
    name = data_spatiotemporal_balance_thinning,
    command = select_spatial_thinning(
      data_source = data_time_controlled_balance_records,
      strata = c("region", "climatezone"),
      distance_km = h1_analysis_config[["spatial_distances_km"]],
      repetitions = h1_analysis_config[["thinning_repetitions"]],
      id_col = "model_id",
      seed = h1_analysis_config[["seed"]]
    )
  ),
  targets::tar_target(
    name = table_spatiotemporal_balance_sensitivity,
    command = summarise_spatial_importance_sensitivity(
      data_records = data_time_controlled_balance_records,
      data_thinning = data_spatiotemporal_balance_thinning
    )
  ),
  targets::tar_target(
    name = table_spatiotemporal_balance_robustness,
    command = classify_spatiotemporal_robustness(
      data_sensitivity = table_spatiotemporal_balance_sensitivity,
      data_spatial_estimates = table_spatiotemporal_balance_estimates,
      data_human_climate_only =
        table_human_climate_only_matched_estimates
    )
  )
)
