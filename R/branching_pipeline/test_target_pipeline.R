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

Sys.setenv(TAR_PROJECT = "project_test")

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

# Need to define a data.frame, where each row will be itirated over.
#   Not that this can be done also using `tidyr::expand_grid()` for more
#   flexibility.
region_df <-
  tibble::tibble(
    group = c(
      "Europe",
      "Latin America",
      "Asia",
      "Africa",
      "North America",
      "Oceania"
    )
  )

# Create a list of targets to be uised in te pileline.
# `tarchetypes::tar_map()` will created a list of targets and becase pipeline
# is just list of targets, it can be inserted in the pipeline.
# It is important to set `unlist = FALSE` and define it BEFORE teh pipe so
# that it can be used to combine results later
tar_mapped <-
  tarchetypes::tar_map(
    unlist = FALSE,
    values = region_df,
    names = group,
    targets::tar_target(
      name = data_subset,
      command = get_data_subset(data_dummy_merge, group),
    )
  )

# the targets list:
list(
  # some dummy age dataframe
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
  # vector with regions
  targets::tar_target(
    name = region_vec,
    command = c(
      "Europe",
      "Latin America",
      "Asia",
      "Africa",
      "North America",
      "Oceania"
    )
  ),
  # dummy data combining the age and region. This should simulate data with
  # values in different regions
  targets::tar_target(
    name = data_dummy_merge,
    command = tidyr:::expand_grid(
      group = region_vec,
      age = data_dummy_time
    )
  ),
  # Static branching -----
  # here is inserted the list of all targets defined previously
  tar_mapped,
  # Combine all targets
  tarchetypes::tar_combine(
    data_combined_static,
    tar_mapped[["data_subset"]],
    command = dplyr::bind_rows(!!!.x, .id = "region")
  ),
  # dynamic branching -----
  targets::tar_target(
    name = data_combined_dynamic,
    command = get_data_subset(data_dummy_merge, region_vec),
    pattern = map(region_vec)
  ),
  # compare results
  targets::tar_target(
    name = data_compare,
    command = waldo::compare(
      x = data_combined_dynamic,
      x_arg = "dynamic",
      y = data_combined_static,
      y_arg = "static"
    )
  )
)
