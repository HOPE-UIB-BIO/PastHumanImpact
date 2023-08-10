#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                      Hypothesis I
#
#
#                   O. Mottl, V. Felde
#                         2023
#
#----------------------------------------------------------#

#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

library(here)

# Load configuration
source(
  here::here(
    "R/00_Config_file.R"
  )
)

Sys.setenv(TAR_PROJECT = "project_h2")

targets::tar_option_set(
  packages = package_list, # [config]
  memory = "transient",
  garbage_collection = TRUE,
  repository = "local",
  seed = set_seed, # [config]
  storage = "worker"
)

#----------------------------------------------------------#
# 1. Load and prepare ecozone data -----
#----------------------------------------------------------#

# This step is important for cretaing a static branching pipeline
#   AKA making many targets at the same time withoud specifically declering them

data_for_hvar <-
  targets::tar_read(
    name = "data_for_hvar",
    store = paste0(
      data_storage_path,
      "_targets_h1"
    )
  )

data_meta <-
  targets::tar_read(
    name = "data_meta",
    store = paste0(
      data_storage_path,
      "_targets_h1"
    )
  )

data_ecozone <-
  data_meta %>%
  dplyr::inner_join(
    data_for_hvar %>%
      dplyr::distinct(dataset_id),
    by = "dataset_id"
  ) %>%
  dplyr::distinct(
    region, ecozone_koppen_15, dataset_id
  )

data_valid_ecozones <-
  data_ecozone %>%
  dplyr::group_by(region, ecozone_koppen_15) %>%
  dplyr::summarise(
    .groups = "drop",
    n_datasets = n_distinct(dataset_id)
  ) %>%
  dplyr::filter(n_datasets > 5) %>%
  dplyr::arrange(n_datasets) %>%
  dplyr::select(-n_datasets)

#----------------------------------------------------------#
# 1. Targets -----
#----------------------------------------------------------#

# the targets list:
list(
  targets::tar_target(
    name = max_temporal_k,
    command = 12
  ),
  targets::tar_target(
    name = vec_predictors,
    command = c(
      "temp_annual",
      "temp_cold",
      "prec_annual",
      "prec_summer",
      "prec_win",
      "spd"
    )
  ),
  targets::tar_target(
    name = data_pred_errors,
    command = tibble::tibble(
      predictor = vec_predictors,
      error_family = c(
        rep(
          "stats::gaussian(link = 'identity')", ,
          2
        ),
        rep(
          "stats::Gamma(link = 'log')", 3
        ),
        "mgcv::tw(link = 'log')"
      )
    )
  ),
  targets::tar_target(
    name = "data_merge",
    command = dplyr::inner_join(
      data_ecozone,
      data_for_hvar
    )
  ),
  targets::tar_target(
    name = "data_merge_unnest",
    command = tidyr::unnest(
      data_merge, data_merge
    )
  )
)
