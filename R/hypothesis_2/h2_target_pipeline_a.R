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

vec_predictors <-
  c(
    "temp_annual",
    "temp_cold",
    "prec_annual",
    "prec_summer",
    "prec_win",
    "spd"
  )

#----------------------------------------------------------#
# 1. Load and data -----
#----------------------------------------------------------#

# These are targets from H1

data_for_hvar <-
  targets::tar_read(
    name = "data_hvar_filtered",
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


#----------------------------------------------------------#
# 2. Prepare ecozone data -----
#----------------------------------------------------------#

# This step is important for cretaing a static branching pipeline
#   AKA making many targets at the same time withoud specifically declering them


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
  dplyr::arrange(n_datasets)

data_dummy_ecozone_predictor <-
  tidyr::expand_grid(
    predictor = vec_predictors,
    data_valid_ecozones
  ) %>%
  dplyr::mutate(
    region_zone = paste(region, ecozone_koppen_15, sep = "_"),
    full_name = paste(predictor, region_zone, sep = "_"),
    data_name = paste0("data_to_fit_mod_", full_name)
  )


#----------------------------------------------------------#
# 3. Prepare target factory for hGAM fitting -----
#----------------------------------------------------------#

tar_mapped_data <-
  tarchetypes::tar_map(
    unlist = FALSE,
    values = data_dummy_ecozone_predictor,
    names = full_name,
    targets::tar_target(
      name = data_to_fit_mod,
      command = get_data_for_indiv_hgams(
        data_raw = data_merge_unnest,
        data_error = data_pred_errors,
        sel_region = region,
        sel_group = ecozone_koppen_15,
        y_var = predictor,
        sel_k = max_temporal_k
      ),
    )
  )

#----------------------------------------------------------#
# 4. Target pipeline -----
#----------------------------------------------------------#

# the targets list:
list(
  targets::tar_target(
    name = max_temporal_k,
    command = 12
  ),
  targets::tar_target(
    name = vec_predictors,
    command = vec_predictors
  ),
  targets::tar_target(
    name = data_pred_errors,
    command = tibble::tibble(
      predictor = vec_predictors,
      error_family = c(
        rep(
          "stats::gaussian(link = 'identity')",
          2
        ),
        rep(
          "mgcv::tw(link = 'log')", 3
        ),
        "mgcv::Tweedie(p = 1.1, link = 'log')"
      )
    )
  ),
  targets::tar_target(
    name = data_merge,
    command = dplyr::inner_join(
      data_ecozone,
      data_for_hvar
    ) %>%
      dplyr::rename(
        group = ecozone_koppen_15
      )
  ),
  targets::tar_target(
    name = data_merge_unnest,
    command = tidyr::unnest(
      data_merge, data_merge
    )
  ),
  # prepare data for hGAM
  tar_mapped_data
)