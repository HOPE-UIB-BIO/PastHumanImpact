#----------------------------------------------------------#
# Run protected temporal-model lifecycle pipelines
#----------------------------------------------------------#

library(here)

source(here::here("R/00_Config_file.R"))

visualise_pipeline <- FALSE

data_pipelines <-
  tibble::tribble(
    ~stage, ~store_relative_path,
    "01_inputs", "temporal_models/inputs",
    "02_configuration", "temporal_models/configuration",
    "03_fitting", "temporal_models/fitting",
    "04_evaluation", "temporal_models/evaluation",
    "05_predictions", "temporal_models/predictions",
    "06_exports", "temporal_models/exports"
  ) |>
  dplyr::mutate(
    script = purrr::map_chr(
      .data[["stage"]],
      ~ here::here(
        "R",
        "analyses",
        "03_temporal_models",
        .x,
        "pipeline.R"
      )
    ),
    store = purrr::map_chr(
      .data[["store_relative_path"]],
      ~ resolve_pipeline_store_path(
        data_storage_path = data_storage_path,
        store_relative_path = .x
      )
    )
  )

purrr::pwalk(
  .l = data_pipelines |>
    dplyr::select(dplyr::all_of(c("script", "store"))),
  .f = ~ run_target_pipeline(
    script = ..1,
    store = ..2,
    visualise = visualise_pipeline
  )
)
