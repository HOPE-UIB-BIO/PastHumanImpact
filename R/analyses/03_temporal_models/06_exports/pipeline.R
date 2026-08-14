#----------------------------------------------------------#
# Temporal-model stable exports
#----------------------------------------------------------#

library(here)

source(here::here("R/00_Config_file.R"))

store_predictions <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "temporal_models/predictions"
  )

runner_temporal <-
  "R/analyses/03_temporal_models/00_run.R"

path_table_dir <-
  here::here("Outputs", "Tables", "Temporal_models")

list(
  targets::tar_target(
    name = data_temporal_model_predictions,
    command = load_target_store_value(
      store = store_predictions,
      target_name = "data_temporal_model_predictions",
      runner = runner_temporal
    )
  ),
  targets::tar_target(
    name = table_pap_temporal_predictions,
    command = data_temporal_model_predictions |>
      dplyr::filter(.data[["analysis"]] == "pap_temporal")
  ),
  targets::tar_target(
    name = file_pap_temporal_predictions,
    command = {
      dir.create(
        path = path_table_dir,
        recursive = TRUE,
        showWarnings = FALSE
      )

      path_output <-
        file.path(path_table_dir, "pap_temporal_predictions.csv")

      readr::write_csv(table_pap_temporal_predictions, path_output)

      path_output
    },
    format = "file"
  )
)
