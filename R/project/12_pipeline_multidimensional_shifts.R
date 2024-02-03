#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                      Hypothesis II
#
#
#                   O. Mottl, V.A. Felde
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

# - Load meta data
source(
  here::here(
    "R/project/02_meta_data.R"
  )
)

#----------------------------------------------------------#
# 1. Load and data -----
#----------------------------------------------------------#


# Load all results from SuperComputer
vec_mods <-
  list.files(
    here::here(data_storage_path, "h2_predictor_jobs"),
    pattern = "mod.rds",
    recursive = TRUE
  )

mod_list <-
  purrr::map(
    .progress = TRUE,
    .x = vec_mods,
    .f = ~ readr::read_rds(
      here::here(
        data_storage_path, "h2_predictor_jobs", .x
      )
    )
  ) %>%
  purrr::set_names(
    nm = stringr::str_replace(vec_mods, "/mod.rds", "")
  )


#----------------------------------------------------------#
# 2. Target pipeline -----
#----------------------------------------------------------#

# the targets list:
list(
  # - path to data for multidimensional shifts ----
  targets::tar_target(
    name = data_m2_path,
    command = paste0(
      data_storage_path,
      "_targets_data/pipeline_paps/objects/data_m2"
    ),
    format = "file"
  ),
  # - load data for multidimensional shifts
  targets::tar_target(
    name = data_m2,
    command = get_file_from_path(data_m2_path)
  ),
  # - predict all models
  targets::tar_target(
    name = mod_predicted,
    command = get_predition_from_model_list(
      data_source_list = mod_list,
      dummy_table = data_dummy_time
    )
  ),
  # - add names to predicted models
  targets::tar_target(
    name = mod_predicted_with_names,
    command = get_names_from_full_model_name(mod_predicted)
  ),
  # - merge datasets for hvar analyses of multidimensional shifts
  targets::tar_target(
    name = data_for_hvar_h2,
    command = get_data_for_h2_hvar(
      data_m2 = data_m2,
      data_predictors = mod_predicted_with_names
    )
  ),
  # - run hierarchical variation partitioning
  targets::tar_target(
    name = output_hvar_h2_spd,
    command = run_hvarpart(
      data_source = data_for_hvar_h2,
      response_vars = NULL,
      response_dist = NULL,
      data_response_dist = "m2",
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
