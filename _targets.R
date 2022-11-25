# Load packages:
library(targets)
library(tidyverse)

# Define directory for external storage for users
auth_tibble <-
  tibble::tibble(
    name = c("ondre", "omo084", "vfe032", "vfe032", "sfl046", "kbh022"),
    paths = c(
      "C:/Users/ondre/OneDrive - University of Bergen/HOPE_data/",
      "C:/Users/omo084/OneDrive - University of Bergen/HOPE_data/",
      "/Users/vfe032/Library/CloudStorage/OneDrive-SharedLibraries-UniversityofBergen/Ondrej Mottl - HOPE_data/",
      "C:/Users/vfe032/OneDrive - University of Bergen/HOPE_data/",
      "C:/Users/sfl046/University of Bergen/Ondrej Mottl - HOPE_data/",
      "C:/Users/kbh022/University of Bergen/Ondrej Mottl - HOPE_data/"
    )
  )

sys_info <- Sys.info()

username <-
  sys_info["user"]

data_storage_path <-
  auth_tibble %>%
  dplyr::filter(name == username) %>%
  purrr::pluck("paths")

if (length(data_storage_path) > 1) {
  data_storage_path <- data_storage_path[2]
}

external_storage_targets <-
  paste0(
    data_storage_path,
    "HOPE_Hypothesis1/_targets"
  )

# set configuration for _target storage
tar_config_set(
  store = external_storage_targets
)

# Set target options:
tar_option_set(
  packages = c(
    "tidyverse",
    "assertthat",
    "devtools",
    "usethis",
    "here",
    "renv",
    "roxygen2",
    "readr",
    "ggpubr",
    "mgcv",
    "REcopol",
    "RRatepol",
    "RUtilpol",
    "vegan",
    "arrow"
  ),
  memory = "transient",
  garbage_collection = TRUE,
  storage = "worker",
  retrieval = "worker",
  repository = "local"
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# list R functions and source them
invisible(lapply(
  list.files(
    path = "R/functions",
    pattern = "*.R",
    recursive = TRUE,
    full.names = TRUE
  ),
  source
))

# the targets list:
list(
  # 1. Pollen data prepartion -----
  # get path to the data assembly
  targets::tar_target(
    name = file_assembly_path,
    command = paste0(
      data_storage_path,
      "HOPE_Hypothesis1/Data/assembly/data_assembly-2022-05-23.rds"
    ),
    format = "file"
  ),
  # - load data assembly from path
  targets::tar_target(
    name = data_pollen_assembly,
    command = get_data(file_assembly_path)
  ),
  # - filter data
  targets::tar_target(
    name = data_pollen_filtered,
    command = filter_all_data(data_pollen_assembly)
  ),
  # 2. Human events -----
  # - a path for events from diagrams
  targets::tar_target(
    name = events_diag_path,
    command = paste0(
      data_storage_path,
      "HOPE_Hypothesis1/Data/events/events_from_diagrams_2022-11-24.rds"
    ),
    format = "file"
  ),
  # - load events from pollen diagrams
  targets::tar_target(
    name = events_diag_raw,
    command = get_file_from_path(events_diag_path)
  ),
  # - turn events from diagrams into binary
  targets::tar_target(
    name = events_diag_binary,
    command = get_events_as_binary(events_diag_raw, data_pollen_filtered)
  ),
  # add logical rules to the binary values (events from diag)
  targets::tar_target(
    name = events_diag,
    command = add_logical_rules(events_diag_binary)
  ),
  # - a path for indicators (from code)
  targets::tar_target(
    name = events_indicators_path,
    command = paste0(
      data_storage_path,
      "HOPE_Hypothesis1/Data/events/events_from_code_indicators_2022-11-25.rds"
    ),
    format = "file"
  ),
  # - load indicators
  targets::tar_target(
    name = events_indicators_raw,
    command = get_file_from_path(events_indicators_path)
  ),
  # - detect indicators in data
  targets::tar_target(
    name = events_indicators,
    command = detect_events_from_indicators(
      data_source_indicators = events_indicators_raw,
      data_source_pollen = data_pollen_filtered,
      sel_region = "Latin America",
      # filter out pinus in selected  countries where Pinus is native
      country_w_pinus = c(
        "Mexico",
        "Guatemala",
        "Honduras",
        "Nicaragua",
        "Costa Rica"
      )
    )
  ),
  # - a path for indices (from code)
  targets::tar_target(
    name = events_indices_path,
    command = paste0(
      data_storage_path,
      "HOPE_Hypothesis1/Data/events/events_from_code_indices_2022-11-25.rds"
    ),
    format = "file"
  ),
  # - load indices
  targets::tar_target(
    name = events_indices_raw,
    command = get_file_from_path(events_indices_path)
  ),
  # - detect indices in data
  targets::tar_target(
    name = events_indices,
    command = detect_events_from_indices(
      data_source_indices = events_indices_raw,
      data_source_pollen = data_pollen_filtered,
      sel_region = "Latin America"
    )
  ),
  # - merge all events detected by code together
  targets::tar_target(
    name = events_code,
    command = merge_indicators_and_indices(
      data_source_indices = events_indices,
      data_source_indicators = events_indicators
    )
  ),
  # - merge all events together
  targets::tar_target(
    name = events,
    command = merge_all_events(
      data_source_events_diag = events_diag,
      data_source_events_code = events_code
    )
  )
)

# tar_target(data_diversity, get_diversity(data_pollen_filtered)),
# tar_target(data_mrt, get_mrt(data_pollen_filtered)),
# tar_target(data_dcca, get_dcca(data_pollen_filtered)),
# tar_target(data_roc, get_roc(data_pollen_filtered))


# tar_target(data_sites, get_data_site(data_assembly))
# ADD TO TARGETS LIST STEPWISE
# tar_target(data_combined_pap, combine_pap(data_pollen_filtered, data_diversity, data_mrt, data_roc, data_dcca))
# tar_target(data_change_points_pap, get_change_points_pap(data_combined_paps))


# MODIFYING CODE:

# tar_target(data_density, get_density_pap(data_change_points_pap))


# TO BE ADDED
# make a separate run gam function on  response data first or at the end when all variables are in or incorporate in get_data_h1

# tar_target(data_climate, get_climate())
# tar_target(data_spd, get_spd())
# tar_target(data_h1, get_data_h1(data_combined_pap, data_density, data_sites, data_events, data_climate, data_spd))

# tar_target(model_h1, run_model_h1(data_h1))
