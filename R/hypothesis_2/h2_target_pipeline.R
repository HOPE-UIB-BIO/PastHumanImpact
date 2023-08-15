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

vec_responses <-
  c(
    "dataset_id",
    "age",
    "n0",
    "n1",
    "n2",
    "n1_minus_n2",
    "n2_divided_by_n1",
    "n1_divided_by_n0",
    "dcca_axis_1", "roc",
    "density_turnover",
    "density_diversity"
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

data_dummy_time <-
  targets::tar_read(
    name = "data_dummy_time",
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
  # //TODO remove this for full analysis. This is only temporary
  #   to make workable results
  dplyr::filter(n_datasets <= 32) %>%
  dplyr::arrange(n_datasets)

data_dummy_ecozone_predictor <-
  tidyr::expand_grid(
    predictor = vec_predictors,
    data_valid_ecozones
  ) %>%
  dplyr::mutate(
    region_zone = paste(region, ecozone_koppen_15, sep = "_"),
    full_name = paste(predictor, region_zone, sep = "_"),
  )


#----------------------------------------------------------#
# 3. Prepare target factory for hGAM fitting -----
#----------------------------------------------------------#

tar_mapped_models <-
  tarchetypes::tar_map(
    unlist = FALSE,
    values = data_dummy_ecozone_predictor,
    names = full_name,
    targets::tar_target(
      name = mod,
      command = fit_hgam_per_region_and_group(
        data_raw = data_merge_unnest,
        data_error = data_pred_errors,
        sel_region = region,
        sel_group = ecozone_koppen_15,
        y_var = predictor,
        sel_k = max_temporal_k,
        use_parallel = TRUE,
        verbose = TRUE
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
          "mgcv::tw(link = 'log')", 4
        )
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
  # add get data for procrustes sum-of-squares (m2) analysis
  targets::tar_target(
    name = data_m2,
    command = get_data_m2(
      data_source = data_for_hvar,
      data_meta = data_meta,
      min_samples = 5,
      select_vars = vec_responses
    )
  ),
  # fit hGAMs
  tar_mapped_models,
  # combine models
  tarchetypes::tar_combine(
    name = mod_list,
    tar_mapped_models[["mod"]],
    command = list(!!!.x),
    iteration = "list"
  ),
  # predict all models
  targets::tar_target(
    name = mod_predicted,
    command = get_predition_from_model_list(
      data_source_list = mod_list,
      dummy_table = data_dummy_time
    )
  ),
  # add names to predicted models
  targets::tar_target(
    name = mod_predicted_with_names,
    command = get_names_from_full_model_name(mod_predicted)
  ),
  # merge datasets
  targets::tar_target(
    name = data_for_hvar_h2,
    command = get_data_for_h2_hvar(
      data_m2 = data_m2,
      data_predictors = mod_predicted_with_names
    )
  ),
  # hierarchical variation partitioning
  targets::tar_target(
    name = output_hvar_h2,
    command = run_hvarpart(
      data_source = data_for_hvar_h2,
      response_vars = NULL,
      responce_dist = "m2",
      predictor_vars = list(
        human = c("spd"),
        climate = c(
          "temp_annual",
          "temp_cold",
          "prec_summer",
          "prec_win"
        ),
        time = c("age")
      ),
      run_all_predictors = FALSE,
      time_series = TRUE,
      get_significance = FALSE,
      permutations = 999
    )
  )
)
