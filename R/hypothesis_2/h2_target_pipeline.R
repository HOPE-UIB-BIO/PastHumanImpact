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
# 1. Targets -----
#----------------------------------------------------------#

# the targets list:
list(
  # 0. setting variables ----
  targets::tar_target(
    name = data_dummy_time,
    command = tibble::tibble(
      age = seq(
        from = min_age, # [config]
        to = max_age, # [config]
        by = timestep # [config]
      )
    )
  ),
  targets::tar_target(
    name = age_cutoff_region,
    command = tibble::tibble(
      region = c(
        "Europe",
        "Latin America",
        "Asia",
        "Africa",
        "North America",
        "Oceania"
      ),
      age_from = c(2000, 2000, 2000, 2000, 500, 500),
      age_end = rep(8500, 6)
    )
  ),
  targets::tar_target(
    name = spd_distance_vec,
    command = c(5, 25, 50, 100, 250, 500) %>%
      rlang::set_names()
  )
)
