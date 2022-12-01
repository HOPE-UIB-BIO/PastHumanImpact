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
    "rcarbon",
    "devtools",
    "geosphere",
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

min_age <- 0
max_age <- 12e3
timestep <- 100


# the targets list:
list(
  # 0. setting variables ----
  targets::tar_target(
    name = data_dummy_time,
    command = tibble::tibble(
      age = seq(
        from = min_age,
        to = max_age,
        by = timestep
      )
    )
  ),
  targets::tar_target(
    name = spd_distance_vec,
    command = c(5, 25, 50, 100, 250, 500) %>%
      rlang::set_names()
  ),
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
    name = data_assembly,
    command = get_data(file_assembly_path)
  ),
  # - filter data
  targets::tar_target(
    name = data_assembly_filtered,
    command = filter_all_data(data_assembly)
  ),
  # 2 Split data into pollen and site data -----
  # -- select relevant data and relevant variables for PAP estimation and
  #     add percentages
  targets::tar_target(
    name = data_pollen,
    command = select_data(
      data_assembly_filtered,
      variables = c(
        "dataset_id",
        "counts_harmonised",
        "levels",
        "age_uncertainty",
        "pollen_percentage",
        "end_of_interest_period"
      ),
      add_percentages = TRUE
    )
  ),
  # -- select only relevant meta data for dataset_id
  targets::tar_target(
    name = data_meta,
    command = select_data(
      data_assembly_filtered,
      variables = c(
        "dataset_id",
        "handle",
        "country",
        "long",
        "lat",
        "altitude",
        "depositionalenvironment",
        "region",
        "curve_name",
        "ecozone_koppen_5",
        "ecozone_koppen_15",
        "ecozone_koppen_30",
        "data_publicity",
        "doi"
      ),
      add_percentages = FALSE
    )
  ),
  # 3. Human events -----
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
    command = get_events_as_binary(events_diag_raw, data_pollen)
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
    command = get_events_from_indicators(
      data_source_indicators = events_indicators_raw,
      data_source_pollen = data_pollen,
      data_source_meta = data_meta,
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
    command = get_events_from_indices(
      data_source_indices = events_indices_raw,
      data_source_pollen = data_pollen,
      data_source_meta = data_meta,
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
  ),
  # - expand events to be present for each time slice
  targets::tar_target(
    name = event_temporal_spacing,
    command = get_per_timeslice_all_col(
      data_source = events,
      data_source_dummy_time = data_dummy_time,
      sel_name = "event_type",
      col_to_unnest = "events_updated",
      smooth_basis = "cr",
      error_family = "stats::binomial(link = 'logit')",
      max_k = round(max(data_dummy_time$age) / 500)
    )
  ),
  # - subset event types relevant for each region
  targets::tar_target(
    name = events_temporal_subset,
    command = subset_event_types(
      data_source_events = event_temporal_spacing,
      data_source_meta = data_meta,
      data_source_dummy_time = data_dummy_time
    )
  ),
  # 4. C14 and SPD -----
  # - create a circle polygons within a certain distance from each site
  targets::tar_target(
    name = data_polygons,
    command = get_polygons(
      data_source = data_meta,
      distance_buffer = 10 # 10° away from site
    )
  ),
  # - a path for c14 data
  targets::tar_target(
    name = data_c14_path,
    command = paste0(
      data_storage_path,
      "HOPE_Hypothesis1/Data/c14/data_rc_2022-11-29.rds"
    ),
    format = "file"
  ),
  # - load c14 data
  targets::tar_target(
    name = data_c14,
    command = get_file_from_path(data_c14_path)
  ),
  # - subset C14 data for each dataset_id and calculate distance to it
  targets::tar_target(
    name = data_c14_subset,
    command = subset_c14_data(
      data_source_c14 = data_c14,
      data_source_polygons = data_polygons,
      data_source_meta = data_meta
    )
  ),
  # - estimaet spd for each distance
  targets::tar_target(
    name = data_spd,
    command = get_spd(
      data_source_c14 = data_c14_subset,
      data_source_dist_vec = spd_distance_vec,
      age_from = min_age,
      age_to = max_age,
      age_timestep = timestep,
      min_n_dates = 50
    )
  ),
  # get spd values for each time slice
  targets::tar_target(
    name = data_sdp_temporal_spacing,
    command = get_per_timeslice_all_col(
      data_source = data_spd,
      data_source_dummy_time = data_dummy_time,
      sel_name = "distance",
      col_to_unnest = "spd",
      smooth_basis = "cr",
      error_family = "mgcv::betar(link = 'logit')",
      max_k = round(max(data_dummy_time$age) / 500)
    )
  ),
  targets::tar_target(
    name = data_spd_best_dist,
    command = select_best_spd(
      data_source_events = events_temporal_subset,
      data_source_spd = data_sdp_temporal_spacing,
      data_source_meta = data_meta,
      data_source_dist_vec = spd_distance_vec
    )
  ),
  # 5. Estimate PAPs -----
  # - calculate diversity
  targets::tar_target(
    # note cannot reuse existing targets name, any solution?
    name = data_diversity,
    command = get_diversity(
      data_pollen,
      n_rand = 999,
      sel_method = "taxonomic"
    )
  ),
  # - run multivariate regression trees (MRT) to estimate compositional change
  # - use percentages without prior transformation
  targets::tar_target(
    name = data_mrt,
    command = get_mrt(
      data_pollen,
      n_rand = 999,
      transformation_coef = "chisq"
    )
  ),
  # - run detrended canonical correspondence analysis (DCCA) to estimate
  #     compositional turnover
  # - use percentages without prior transformations
  targets::tar_target(
    name = data_dcca,
    command = get_dcca(
      data_pollen,
      sel_method = "constrained",
      var_name_pred = "age",
      sel_complexity = "poly_2",
      transform_to_percentage = FALSE,
      tranformation = "none"
    )
  ),
  # - calculate Rate-of-change (RoC)
  targets::tar_target(
    name = data_roc,
    command = get_roc(
      data_pollen,
      smoothing_method = "age.w",
      min_points_smoothing = 5,
      max_points_smoothing = 9,
      age_range_smoothing = 500,
      working_units_selection = "MW",
      size_of_bin = 500,
      n_mowing_windows = 5,
      which_level_select_in_bin = "random",
      n_rand = 1000,
      n_individuals_to_standardise = 150,
      transformation_coef = "chisq",
      peak_point_method = "trend_non_linear",
      sd_for_peak_detection = 2
    )
  ),
  # - combine all PAP estimates into one tibble
  targets::tar_target(
    name = data_combined_paps,
    command = combine_pap(
      data_pollen,
      data_diversity,
      data_mrt,
      data_roc,
      data_dcca
    )
  ),
  # - calculate change points of all PAP variables by regression trees (RT)
  targets::tar_target(
    name = data_change_points,
    command = get_change_points_pap(data_combined_paps)
  )
)




# MODIFYING CODE:

# tar_target(data_density, get_density_pap(data_change_points_pap))


# TO BE ADDED
# make a separate run gam function on  response data first or at the end when all variables are in or incorporate in get_data_h1

# tar_target(data_climate, get_climate())
# tar_target(data_spd, get_spd())
# tar_target(data_h1, get_data_h1(data_combined_pap, data_density, data_sites, data_events, data_climate, data_spd))

# tar_target(model_h1, run_model_h1(data_h1))
