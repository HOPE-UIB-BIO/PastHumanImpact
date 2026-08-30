#----------------------------------------------------------#
# Run ordered project data preparation
#----------------------------------------------------------#

library(here)

source(here::here("R/00_Config_file.R"))

visualise_pipeline <- FALSE

source(
  here::here(
    "R/analyses/01_data_preparation/01_metadata/01_filter_metadata.R"
  )
)

source(
  here::here(
    "R/analyses/01_data_preparation/02_climate/download_climate_data.R"
  )
)

data_pipelines <-
  tibble::tribble(
    ~script_relative_path, ~store_relative_path,
    "03_archaeological_proxies/spd/pipeline.R", "data_preparation/spd",
    "04_pollen/pipeline.R", "data_preparation/pollen",
    "05_paps/pipeline.R", "data_preparation/paps",
    "03_archaeological_proxies/events/pipeline.R",
    "data_preparation/events",
    "06_predictors/pipeline.R", "data_preparation/predictors"
  ) |>
  dplyr::mutate(
    script = purrr::map_chr(
      .data[["script_relative_path"]],
      ~ here::here("R", "analyses", "01_data_preparation", .x)
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
