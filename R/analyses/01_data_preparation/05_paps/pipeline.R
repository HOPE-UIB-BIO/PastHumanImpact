#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#               Pollen-derived property preparation
#
#
#                   O. Mottl, V.A. Felde
#                         2024
#
#----------------------------------------------------------#
# Defines the pollen-derived property preparation target graph.
# Run with:
#   R/analyses/01_data_preparation/00_run.R
# Sourcing this script only declares targets; it does not execute them.

#----------------------------------------------------------#
# 0. Configure pipeline -----
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
    "R/analyses/01_data_preparation/01_metadata/02_metadata.R"
  )
)

#----------------------------------------------------------#
# 1. Upstream contract -----
#----------------------------------------------------------#

store_pollen <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "data_preparation/pollen"
  )

runner_data_preparation <-
  "R/analyses/01_data_preparation/00_run.R"

path_mvpart_runner <-
  here::here(
    "R/analyses/01_data_preparation/05_paps/run_mvpart_runtime.R"
  )

path_mvpart_installer <-
  here::here(
    "R/analyses/01_data_preparation/05_paps/install_mvpart_runtime.R"
  )

paths_mvpart_functions <-
  here::here(
    "R/functions/human_impact/paps",
    c(
      "prepare_pollen_percentages.R",
      "compute_chi_square_standardisation.R",
      "prepare_transformed_pollen_composition.R",
      "normalise_partition_groups.R",
      "compute_mvpart_change_points.R",
      "fit_mvpart_mrt.R",
      "fit_mvpart_regression_tree.R",
      "fit_pap_change_point_dataset.R",
      "compute_mrt.R",
      "compute_pap_change_points.R"
    )
  )

#----------------------------------------------------------#
# 1. Define targets -----
#----------------------------------------------------------#

list(
  # Why: Track the old-R runner so changes invalidate its dependent results.
  targets::tar_target(
    name = "file_mvpart_runner",
    command = path_mvpart_runner,
    format = "file"
  ),
  # Why: Track the pinned installer as part of runtime provenance.
  targets::tar_target(
    name = "file_mvpart_installer",
    command = path_mvpart_installer,
    format = "file"
  ),
  # Why: Track isolated functions so changes invalidate old-R calculations.
  targets::tar_target(
    name = "files_mvpart_functions",
    command = paths_mvpart_functions,
    format = "file"
  ),
  # Why: Fingerprint pollen so upstream changes invalidate this pipeline store.
  targets::tar_target(
    name = "fingerprint_pollen",
    command = compute_target_store_fingerprint(
      store = store_pollen,
      target_names = "data_pollen",
      runner = runner_data_preparation
    ),
    cue = targets::tar_cue(mode = "always")
  ),
  # Why: Prepare pollen so downstream targets share one canonical dataset.
  targets::tar_target(
    name = "data_pollen",
    command = {
      fingerprint_pollen

      load_target_store_value(
        store = store_pollen,
        target_name = "data_pollen",
        runner = runner_data_preparation
      )
    }
  ),
  # 3. Estimate PAPs -----
  # - calculate diversity
  # Why: Prepare diversity so downstream targets share one canonical dataset.
  targets::tar_target(
    name = "data_diversity",
    command = compute_diversity(
      data_pollen,
      n_rand = 999,
      sel_method = "taxonomic"
    )
  ),
  # - run detrended canonical correspondence analysis (DCCA) to estimate
  #     compositional turnover
  # - use percentages without prior transformations
  # Why: Prepare dcca so downstream targets share one canonical dataset.
  targets::tar_target(
    name = "data_dcca",
    command = compute_dcca(
      data_pollen,
      sel_method = "constrained",
      var_name_pred = "age",
      sel_complexity = "poly_2",
      transform_to_percentage = FALSE,
      tranformation = "none"
    )
  ),
  # - calculate Rate-of-change (RoC)
  # Why: Prepare roc so downstream targets share one canonical dataset.
  targets::tar_target(
    name = "data_roc",
    command = compute_roc(
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
  # Why: Run MRT in isolated R 3.5 because mvpart cannot load in modern R.
  targets::tar_target(
    name = "result_mrt_runtime",
    command = run_mvpart_runtime(
      operation = "mrt",
      data_input = data_pollen,
      path_runner = file_mvpart_runner,
      path_installer = file_mvpart_installer,
      path_function_files = files_mvpart_functions
    )
  ),
  # Why: Expose the stable MRT table under its existing public target name.
  targets::tar_target(
    name = "data_mrt",
    command = result_mrt_runtime[["data"]]
  ),
  # Why: Preserve the exact legacy runtime used for MRT reproducibility.
  targets::tar_target(
    name = "metadata_mrt_runtime",
    command = result_mrt_runtime[["provenance"]]
  ),
  # - combine all PAP estimates into one tibble for get change-points
  # Why: Prepare prepared cp so downstream targets share one canonical dataset.
  targets::tar_target(
    name = "data_prepared_cp",
    command = prepare_data_cp(
      data_pollen,
      data_diversity,
      data_mrt,
      data_roc,
      data_dcca
    )
  ),
  # - calculate change points of all PAP variables by regression trees (RT)
  # Why: Run change-point trees in isolated R 3.5 for mvpart compatibility.
  targets::tar_target(
    name = "result_change_point_runtime",
    command = run_mvpart_runtime(
      operation = "change_points",
      data_input = data_prepared_cp,
      path_runner = file_mvpart_runner,
      path_installer = file_mvpart_installer,
      path_function_files = files_mvpart_functions
    )
  ),
  # Why: Expose the stable change-point table under its existing target name.
  targets::tar_target(
    name = "data_change_points",
    command = result_change_point_runtime[["data"]]
  ),
  # Why: Preserve the exact legacy runtime used for reproducibility.
  targets::tar_target(
    name = "metadata_change_point_runtime",
    command = result_change_point_runtime[["provenance"]]
  ),
  # - calculate density of change points
  # Why: Prepare density estimate so downstream targets share one canonical
  #   dataset.
  targets::tar_target(
    name = "data_density_estimate",
    command = aggregate_density_pap(
      data_source_change_points = data_change_points,
      data_source_meta = data_meta,
      data_source_dummy_time = data_dummy_time,
      limit_length = TRUE
    )
  ),
  # 4. Combine PAP data -----
  # - merge diversity and DCCA and prepare for modelling
  # Why: Prepare diversity and dcca so downstream targets share one canonical
  #   dataset.
  targets::tar_target(
    name = "data_diversity_and_dcca",
    command = prepare_diversity_dcca_model_data(
      data_source_diversity = data_diversity,
      data_source_dcca = data_dcca,
      data_source_pollen = data_pollen
    )
  ),
  # - estimate diversity and DCCA on equal time slices
  # Why: Prepare div dcca interpolated so downstream targets share one canonical
  #   dataset.
  targets::tar_target(
    name = "data_div_dcca_interpolated",
    command = prepare_interpolated_model_data(
      data_source = data_diversity_and_dcca,
      variable = "var_name",
      vars_interpolate = c("age", "value"),
      group_var = "dataset_id",
      method = "linear",
      rule = 1,
      ties = mean,
      age_min = 0,
      age_max = 12e03,
      timestep = 500,
      verbose = TRUE
    )
  ),
  # - prepare RoC for modelling
  # Why: Prepare roc for modelling so downstream targets share one canonical
  #   dataset.
  targets::tar_target(
    name = "data_roc_for_modelling",
    command = prepare_roc_model_data(data_roc)
  ),
  # - estimate RoC on equal time slices
  # Why: Prepare roc interpolated so downstream targets share one canonical
  #   dataset.
  targets::tar_target(
    name = "data_roc_interpolated",
    command = prepare_interpolated_model_data(
      data_source = data_roc_for_modelling,
      variable = "var_name",
      vars_interpolate = c("age", "value"),
      group_var = "dataset_id",
      method = "linear",
      rule = 1,
      ties = mean,
      age_min = 0,
      age_max = 12e03,
      timestep = 500,
      verbose = TRUE
    )
  ),
  # - merge PAPs together
  # Why: Prepare properties so downstream targets share one canonical dataset.
  targets::tar_target(
    name = "data_properties",
    command = summarise_data_properties(
      data_source_diversity = data_div_dcca_interpolated,
      data_source_roc = data_roc_interpolated,
      data_source_density = data_density_estimate,
      used_rescale = TRUE
    )
  ),
  # - filter data properties for analyses ----
  # Why: Prepare properties filtered so downstream targets share one canonical
  #   dataset.
  targets::tar_target(
    name = "data_properties_filtered",
    command = prepare_filtered_hvarpart_data(
      data_source = data_properties,
      data_meta = data_meta,
      age_from = 2000,
      age_to = 8500,
      remove_private = TRUE
    )
  ),
  # - get data multidimensional shifts (procrustes m2)
  # Why: Prepare m2 filtered so downstream targets share one canonical dataset.
  targets::tar_target(
    name = "data_m2_filtered",
    command = prepare_m2_data(
      data_source = data_properties_filtered,
      data_meta = data_meta,
      min_samples = 5,
      select_vars = c(
        "dataset_id",
        "age",
        "n0",
        "n1",
        "n2",
        "n1_minus_n2",
        "n2_divided_by_n1",
        "n1_divided_by_n0",
        "roc",
        "dcca_axis_1",
        "density_diversity",
        "density_turnover"
      )
    )
  )
) # end of targets
