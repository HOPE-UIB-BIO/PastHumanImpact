#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#              Run the single-proxy H1 pipelines
#
#
#                   O. Mottl, V.A. Felde
#                         2026
#
#----------------------------------------------------------#
# Builds the shared inputs, fits spatial and temporal H1 once per
#   single-proxy profile (each in its own store analyses_h1/<profile_id>),
#   and assembles the outcome table.

#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

library(here)

source(here::here("R/00_Config_file.R"))

visualise_pipeline <- FALSE

path_human_proxies <-
  here::here("R", "analyses", "02_h1_spatiotemporal_hvarpart", "human_proxies")

data_profiles <-
  load_analysis_profiles(
    here::here("R", "analyses", "00_profiles", "analysis_profiles.csv")
  ) |>
  dplyr::filter(
    .data[["configuration_reference"]] == "human_proxy_h1",
    .data[["enabled"]]
  )

#----------------------------------------------------------#
# 1. List pipelines in dependency order -----
#----------------------------------------------------------#

data_runs <-
  data_profiles |>
  dplyr::transmute(
    script_relative_path = dplyr::if_else(
      .data[["analytical_unit"]] == "within_dataset",
      "02_time_control/pipeline.R",
      "03_spatial_control/pipeline.R"
    ),
    store_relative_path = file.path("analyses_h1", .data[["profile_id"]])
  )

data_pipelines <-
  dplyr::bind_rows(
    tibble::tibble(
      script_relative_path = "01_inputs/pipeline.R",
      store_relative_path = "analyses_h1/human_proxy_inputs"
    ),
    data_runs,
    tibble::tibble(
      script_relative_path = "04_outcomes/pipeline.R",
      store_relative_path = "analyses_h1/human_proxy_outcomes"
    )
  ) |>
  dplyr::mutate(
    script = file.path(path_human_proxies, .data[["script_relative_path"]]),
    store = purrr::map_chr(
      .data[["store_relative_path"]],
      ~ resolve_pipeline_store_path(
        data_storage_path = data_storage_path,
        store_relative_path = .x
      )
    )
  )

#----------------------------------------------------------#
# 2. Run pipelines -----
#----------------------------------------------------------#

purrr::pwalk(
  .l = data_pipelines |>
    dplyr::select(dplyr::all_of(c("script", "store"))),
  .f = ~ run_target_pipeline(
    script = ..1,
    store = ..2,
    visualise = visualise_pipeline
  )
)
