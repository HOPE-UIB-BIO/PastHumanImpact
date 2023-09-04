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


# Load all results from SuperComputer
vec_mods <-
  list.files(
    here::here("jobs"),
    pattern = "mod.rds",
    recursive = TRUE
  )

mod_list <-
  purrr::map(
    .progress = TRUE, ,
    .x = vec_mods,
    .f = ~ readr::read_rds(
      here::here(
        "jobs", .x
      )
    )
  ) %>%
  purrr::set_names(
    nm = stringr::str_replace(vec_mods, "/mod.rds", "")
  )


#----------------------------------------------------------#
# 4. Target pipeline -----
#----------------------------------------------------------#

# the targets list:
list(
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
