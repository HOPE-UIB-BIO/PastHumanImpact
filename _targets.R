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
    "arrow",
    "assertthat",
    "devtools",
    "geosphere",
    "ggpubr",
    "here",
    "mgcv",
    "rcarbon",
    "readr",
    "REcopol",
    "renv",
    "roxygen2",
    "RRatepol",
    "RUtilpol",
    "terra",
    "tidyverse",
    "usethis",
    "vegan",
    "rdacca.hp",
    "ggimage",
    "ggforce",
    "venneuler"
  ),
 # error = "null",
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
timestep <- 500


age_end_cutoff <- 85e2 



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
    name = age_cutoff_region,
    command = tibble::tibble(
      region = c("Europe", "Latin America", "Asia", "Africa", "North America", "Oceania"),
      age_from = c(2000, 2000, 2000, 2000, 500, 500)
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
      "HOPE_Hypothesis1/Data/assembly/data_assembly_V2-2022-05-23.rds"
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
    command = get_pollen_data(
      data_assembly = data_assembly_filtered,
      variables = c(
        "dataset_id",
        "counts_harmonised",
        "levels",
        "age_uncertainty",
        "pollen_percentage"
      )
    )
  ),
  # -- select only relevant meta data for dataset_id
  targets::tar_target(
    name = data_meta,
    command = get_meta_data(
      data_assembly = data_assembly_filtered,
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
      )
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
  # - prepare events for modelling
  targets::tar_target(
    name = data_events_to_fit,
    command = get_events_for_modelling(events)
  ),
  # - interpolate data for even time steps
  targets::tar_target(
    name = events_interpolated,
    command = get_interpolated_data(
      data_source = data_events_to_fit,
      variable = "var_name",
      vars_interpolate = c("age", "value"),
      group_var = "dataset_id",
      method = "constant",  
      rule = 1:2,
      ties = "ordered",
      age_min = 0,
      age_max = 12e03,
      timestep = 500,
      verbose = TRUE
    )
  ),
  # - expand events to be present for each time slice
  # targets::tar_target(
  #   name = event_temporal_spacing,
  #   command = get_per_timeslice(
  #     data_source = data_events_to_fit,
  #     data_source_dummy_time = data_dummy_time,
  #     smooth_basis = "cr",
  #     data_error_family = "stats::binomial(link = 'logit')",
  #     max_k = round(max(data_dummy_time$age) / 500),
  #     # interpolate not forecast
  #     limit_length = TRUE,
  #     data_source_meta = data_meta
  #   )
  # ),
  # # - subset event types relevant for each region 
  targets::tar_target(
    name = events_temporal_subset,
    command = subset_event_types(
      data_source_events = events_interpolated,
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
      data_meta = data_meta,
      age_cutoff_region = age_cutoff_region,
      age_to = max_age,
      age_timestep = timestep,
      min_n_dates = 50
    )
  ),
  # - prepare spd for modelling
  targets::tar_target(
    name = data_spd_to_fit,
    command = get_spd_for_modelling(data_spd)
  ),
  # - get interpolated spd values for each time slice
  targets::tar_target(
    name = data_spd_interpolated,
    command = get_interpolated_data(
      data_source = data_spd_to_fit,
      variable = "var_name",
      vars_interpolate = c("age", "value"),
      group_var = "dataset_id",
      method = "linear",
      rule = 1:2,
      ties = mean,
      age_min = 0,
      age_max = 12e03,
      timestep = 500,
      verbose = TRUE
    )
  ),
  # # get spd values for each time slice
  # # targets::tar_target(
  # #   name = data_sdp_temporal_spacing,
  # #   command = get_per_timeslice(
  # #     data_source = data_spd_to_fit,
  # #     data_error_family = "stats::binomial(link = 'logit')",
  # #     data_source_dummy_time = data_dummy_time,
  # #     smooth_basis = "cr",
  # #     max_k = round(max(data_dummy_time$age) / 500),
  # #     weights_var = NULL,
  # #     limit_length = FALSE
  # #   )
  # # ),
  targets::tar_target(
    name = data_spd_best_dist,
    command = select_best_spd(
      data_source_events = events_temporal_subset,
      data_source_spd = data_spd_interpolated,
      data_source_meta = data_meta,
      data_source_dist_vec = spd_distance_vec
    )
  ),
  # - add SPD value for records without humans
  targets::tar_target(
    name = data_spd_full,
    command = add_missing_spd_values(
      data_source_spd = data_spd_best_dist,
      data_source_meta = data_meta,
      data_source_dummy_time = data_dummy_time
    )
  ),
  # 5. Get CHELSA palaeoclimate -----
  # - a path to time reference table (from code)
  targets::tar_target(
    name = time_ref_path,
    command = paste0(
      data_storage_path,
      "HOPE_Hypothesis1/Data/climate/time_reference_table.rds"
    ),
    format = "file"
  ),
  # - load table
  targets::tar_target(
    name = time_ref_table,
    command = get_file_from_path(time_ref_path)
  ),
  targets::tar_target(
    name = data_climate_chelsa,
    command = get_climate_data(
      variables_selected = c("bio", "tasmin"),
      bio_var_selected = c(1, 6, 12, 15, 18, 19),
      time_var_selected = c(20:-200),
      month_var_selected = c(1:12),
      xy = data_meta
    )
  ), 
  targets::tar_target(
    name = data_climate,
    command = get_climate_indices(
      data_source = data_climate_chelsa,
      time_ref = time_ref_table
    )
  ),
  targets::tar_target(
    name = data_climate_for_interpolation,
    command = get_climate_data_for_interpolation(
      data_source = data_climate,
      sel_var = c("temp_annual",
                  "temp_cold",
                  "prec_annual", 
                  "prec_summer", 
                  "prec_win")
      )
  ),
  targets::tar_target(
    name = data_climate_interpolated,
    command = get_interpolated_data(
      data_source = data_climate_for_interpolation,   
      variable = "var_name",
      vars_interpolate = c("age", "value"),
      group_var = "dataset_id",
      method = "linear", 
      rule = 1:2,
      ties = mean,
      age_min = 0,
      age_max = 12e03,
      timestep = 500,
      verbose = TRUE
    )
  ),
  # 6. Estimate PAPs -----
  # - calculate diversity
  targets::tar_target(
    name = data_diversity,
    command = get_diversity(
      data_pollen,
      n_rand = 999,
      sel_method = "taxonomic"
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
  # - combine all PAP estimates into one tibble for get change-points
  targets::tar_target(
    name = data_prepared_cp,
    command = prepare_data_cp(
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
    command = get_change_points_pap(data_prepared_cp)
  ),
  # - calculate density of change points
  targets::tar_target(
    name = data_density,
    command = get_density_pap(
      data_source_change_points = data_change_points,
      data_source_meta = data_meta,
      data_source_dummy_time = data_dummy_time,
      limit_length = TRUE
    )
  ),

# - run hgam model to create a common variable for density diversity and 
# turnover
  targets::tar_target(
    name = data_density_variables,
    command = get_hgam_density_vars(
      data_source_density = data_density,
      data_source_meta = data_meta,
      data_source_dummy_time = data_dummy_time,
      diversity_vars = c(
        "n0", "n1", "n2",
        "n2_divided_by_n1", "n1_divided_by_n0"
      ),
      turnover_vars = c(
        "mvrt", "roc", "dcca"
      ),
      used_rescales = TRUE,
      error_family = "mgcv::betar(link = 'logit')",
      smooth_basis = "tp",
      sel_k = round(max(data_dummy_time$age) / 1000),
      limit_length = TRUE
    )
  ),
  # 7. Hypothesis I -----
  # - merge Diveristy and DCCA and prepare for modelling
  targets::tar_target(
    name = data_diversity_and_dcca,
    command = get_diversity_and_dcca_for_modelling(
      data_source_diversity = data_diversity,
      data_source_dcca = data_dcca,
      data_source_pollen = data_pollen
    )
  ),
  # - estimate Diversity and DCCA on equal time slices with linear interpolation
  targets::tar_target(
    name = data_div_dcca_interpolated,
    command = get_interpolated_data(
      data_source = data_diversity_and_dcca,
      variable = "var_name",
      vars_interpolate = c("age", "value"),
      group_var = "dataset_id",
      method = "linear", 
      rule = 1:2,
      ties = mean,
      age_min = 0,
      age_max = 12e03,
      timestep = 500,
      verbose = TRUE
    )
  ),
  # targets::tar_target(
  #   name = data_div_dcca_temporal_spacing,
  #   command = get_per_timeslice(
  #     data_source = data_diversity_and_dcca,
  #     data_error_family = tibble::tribble(
  #       ~"var_name", ~"sel_error",
  #       "n0", "mgcv::Tweedie(p = 1.1)",
  #       "n1", "mgcv::Tweedie(p = 1.1)",
  #       "n2", "mgcv::Tweedie(p = 1.1)",
  #       "n1_minus_n2", "mgcv::Tweedie(p = 1.1)",
  #       "n2_divided_by_n1", "mgcv::betar(link = 'logit')",
  #       "n1_divided_by_n0", "mgcv::betar(link = 'logit')",
  #       "dcca_axis_1", "mgcv::Tweedie(p = 1.1)"
  #     ),
  #     data_source_dummy_time = data_dummy_time,
  #     smooth_basis = "tp",
  #     max_k = round(max(data_dummy_time$age) / 500),
  #     # use propagating uncertainy
  #     weights_var = "var_weight",
  #     # interpolate not forecast
  #     limit_length = TRUE,
  #     data_source_meta = data_meta
  #   )
  # ),
  # - prepare RoC for modelling
  targets::tar_target(
    name = data_roc_for_modelling,
    command = get_roc_for_modelling(data_roc)
  ),
  # - estimate RoC on equal time slices with linear interpolation
  targets::tar_target(
    name = data_roc_interpolated,
    command = get_interpolated_data(
      data_source = data_roc_for_modelling,
      variable = "var_name",
      vars_interpolate = c("age", "value"),
      group_var = "dataset_id",
      method = "linear", 
      rule = 1:2,
      ties = mean,
      age_min = 0,
      age_max = 12e03,
      timestep = 500,
      verbose = TRUE
      )
  ),
  # targets::tar_target(
  #   name = data_roc_temporal_spacing,
  #   command = get_per_timeslice(
  #     data_source = data_roc_for_modelling,
  #     data_error_family = "mgcv::Tweedie(p = 1.1)",
  #     data_source_dummy_time = data_dummy_time,
  #     smooth_basis = "tp",
  #     max_k = round(max(data_dummy_time$age) / 500),
  #     # use propagating uncertainy
  #     weights_var = "var_weight",
  #     # interpolate not forecast
  #     limit_length = TRUE,
  #     data_source_meta = data_meta
  #   )
  # ),
 # - merge all data together, [remember to add density vars when done!!]
  targets::tar_target(
    name = data_hvar,
    command = get_data_for_hvarpar(
      data_source_diversity = data_div_dcca_interpolated,
      data_source_roc = data_roc_interpolated,
     # data_source_density = data_density_variables,
      data_source_spd = data_spd_full,
      data_source_climate = data_climate_interpolated
    )
  ), # filter data by age of interests in regions; filter out Africa 
 targets::tar_target(
   name = data_hvar_filtered,
   command = filter_data_hvar(
     data_source = data_hvar,
     data_meta = data_meta,
     age_table = age_cutoff_region
   )
 ), # restructure data for temporal analysis
 targets::tar_target(
   name = data_hvar_temporal,
   command = get_data_hvar_timebin(
     data_source = data_hvar_filtered,
     data_meta = data_meta
   )
 ), 
 
 # # Main analyses: hierarchical variation partitioning
  # - run first spatial (within core) analysis
  targets::tar_target(
    name = output_hvar_spatial,
    command = run_hvarpart(
      data_source = data_hvar_filtered,
      response_vars = c(
        "n0", "n1", "n2",
        "n1_minus_n2", "n2_divided_by_n1", "n1_divided_by_n0",
        "roc",
        "dcca_axis_1"
        #, "density_diversity", "density_turnover"
      ),
      predictor_vars = list(
        human = c("spd"),
        climate = c(
          "temp_annual",
          "temp_cold",
          "prec_summer",
          "prec_win"),
        time = c("age")
      ),
      run_all_predictors = FALSE,
      time_series = TRUE,
      get_significance = TRUE,
      permutations = 999
    )
  ), # - run second temporal analysis (by age bins in spatial context)
 targets::tar_target(
   name = output_hvar_temporal,
   command = run_hvarpart(
     data_source = data_hvar_temporal,
     response_vars = c(
       "n0", "n1", "n2",
       "n1_minus_n2", "n2_divided_by_n1", "n1_divided_by_n0",
       "roc",
       "dcca_axis_1"
       #, "density_diversity", "density_turnover"
     ),
     predictor_vars = list(
       human = c("spd"),
       climate = c(
         "temp_annual",
         "temp_cold",
         "prec_summer",
         "prec_win")
       ),
     run_all_predictors = FALSE,
     time_series = FALSE,
     get_significance = TRUE,
     permutations = 999
   )
 )

)

