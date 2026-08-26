#----------------------------------------------------------#
# Run canonical H1 spatiotemporal HVarPart pipelines
#----------------------------------------------------------#

library(here)

source(here::here("R/00_Config_file.R"))

visualise_pipeline <- FALSE

data_pipelines <-
  tibble::tribble(
    ~script_relative_path, ~store_relative_path,
    "01_inputs/pipeline.R",
    "analyses_h1/inputs",
    "02_human_climate_only/within_dataset_spd/pipeline.R",
    "analyses_h1/human_climate_only/within_dataset_spd",
    "02_human_climate_only/within_dataset_events/pipeline.R",
    "analyses_h1/human_climate_only/within_dataset_events",
    "02_human_climate_only/time_slice_spd/pipeline.R",
    "analyses_h1/human_climate_only/time_slice_spd",
    "02_human_climate_only/time_slice_events/pipeline.R",
    "analyses_h1/human_climate_only/time_slice_events",
    "03_time_control/spd/pipeline.R",
    "analyses_h1/time_control/spd",
    "03_time_control/events/pipeline.R",
    "analyses_h1/time_control/events",
    "04_spatial_control/spd/pipeline.R",
    "analyses_h1/spatial_control/spd",
    "04_spatial_control/events/pipeline.R",
    "analyses_h1/spatial_control/events",
    "05_spatial_aggregation/spd_human_climate_balance/pipeline.R",
    paste(
      "analyses_h1/spatial_aggregation",
      "spd_human_climate_balance",
      sep = "/"
    ),
    paste(
      "05_spatial_aggregation",
      "events_human_climate_balance/pipeline.R",
      sep = "/"
    ),
    paste(
      "analyses_h1/spatial_aggregation",
      "events_human_climate_balance",
      sep = "/"
    )
  ) |>
  dplyr::mutate(
    script = purrr::map_chr(
      .data[["script_relative_path"]],
      ~ here::here(
        "R",
        "analyses",
        "02_h1_spatiotemporal_hvarpart",
        .x
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
