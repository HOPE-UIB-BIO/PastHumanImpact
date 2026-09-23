#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#          SPD convergence with KK10 and HYDE 3.2
#
#                   O. Mottl, V.A. Felde
#                         2026
#
#----------------------------------------------------------#
# Defines an optional, data-gated sensitivity target graph.
# Run with:
#   R/analyses/91_sensitivity_analyses/human_proxy_convergence/00_run.R
# Sourcing this script only declares targets; it does not execute them.
# Raw external rasters are never copied into the repository.

#----------------------------------------------------------#
# 0. Configure pipeline -----
#----------------------------------------------------------#

library(here)

source(here::here("R/00_Config_file.R"))

runner_data_preparation <- "R/analyses/01_data_preparation/00_run.R"
runner_h1 <- "R/analyses/02_h1_spatiotemporal_hvarpart/00_run.R"

store_spd <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "data_preparation/spd"
  )

store_h1_inputs <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "analyses_h1/inputs"
  )

path_sources <-
  here::here(
    "R",
    "analyses",
    "91_sensitivity_analyses",
    "human_proxy_convergence",
    "proxy_sources.csv"
  )

path_tables <-
  here::here(
    "Outputs",
    "Tables",
    "H1",
    "Sensitivity",
    "Human_proxy_convergence"
  )

path_figures <-
  here::here(
    "Outputs",
    "Figures",
    "H1",
    "Sensitivity",
    "Human_proxy_convergence"
  )

path_raster_cache <-
  file.path(
    data_storage_path,
    "Human_impact",
    "External_proxies",
    "cache"
  )

config_human_proxy <-
  list(
    target_ages = seq(2000, 8000, by = 500),
    primary_bins = 10L,
    alternative_bins = c(5L, 20L),
    minimum_bins = 8L,
    minimum_datasets = 5L,
    minimum_ages = 3L,
    bootstrap_repetitions = 2000L,
    bootstrap_seed = 342L
  )

#----------------------------------------------------------#
# 1. Define targets -----
#----------------------------------------------------------#

list(
  # Why: Track canonical SPD inputs without modifying their upstream store.
  targets::tar_target(
    name = "fingerprint_human_proxy_spd",
    compute_target_store_fingerprint(
      store = store_spd,
      target_names = "data_spd_by_radius",
      runner = runner_data_preparation
    ),
    cue = targets::tar_cue(mode = "always")
  ),
  # Why: Track location metadata without copying its upstream store.
  targets::tar_target(
    name = "fingerprint_human_proxy_metadata",
    compute_target_store_fingerprint(
      store = store_h1_inputs,
      target_names = "data_meta",
      runner = runner_h1
    ),
    cue = targets::tar_cue(mode = "always")
  ),
  # Why: Track the checked-in external-source contract as a file dependency.
  targets::tar_target(
    name = "file_human_proxy_source_manifest",
    path_sources,
    format = "file"
  ),
  # Why: Validate source metadata and fail clearly when raw files are absent.
  targets::tar_target(
    name = "table_human_proxy_source_manifest",
    readr::read_csv(
      file_human_proxy_source_manifest,
      show_col_types = FALSE
    ) |>
      validate_human_proxy_source_manifest(
        data_storage_path = data_storage_path,
        require_files = TRUE
      )
  ),
  # Why: Track the large KK10 raster without copying it into the repository.
  targets::tar_target(
    name = "file_human_proxy_kk10",
    table_human_proxy_source_manifest |>
      dplyr::filter(.data[["source_id"]] == "kk10") |>
      dplyr::pull("file_path"),
    format = "file"
  ),
  # Why: Track the external HYDE population raster as a file dependency.
  targets::tar_target(
    name = "file_human_proxy_hyde",
    table_human_proxy_source_manifest |>
      dplyr::filter(.data[["source_id"]] == "hyde_3_2") |>
      dplyr::pull("file_path"),
    format = "file"
  ),

  # Why: Import strict-radius SPD histories for spatial-support sensitivity.
  targets::tar_target(
    name = "data_human_proxy_spd_by_radius",
    {
      fingerprint_human_proxy_spd
      load_target_store_value(
        store = store_spd,
        target_name = "data_spd_by_radius",
        runner = runner_data_preparation
      )
    }
  ),
  # Why: Derive the focal fallback from the stored radius product so stores
  # created before the public fallback target remain usable without refitting.
  targets::tar_target(
    name = "data_human_proxy_spd_focal",
    prepare_spd_analysis_products(
      data_human_proxy_spd_by_radius
    )[["data_spd_250_with_500_fallback"]]
  ),
  # Why: Standardize record coordinates and regions for matching.
  targets::tar_target(
    name = "data_human_proxy_metadata",
    {
      fingerprint_human_proxy_metadata
      load_target_store_value(
        store = store_h1_inputs,
        target_name = "data_meta",
        runner = runner_h1
      ) |>
        dplyr::transmute(
          dataset_id = as.character(.data[["dataset_id"]]),
          region = as.character(.data[["region"]]),
          long = as.numeric(.data[["long"]]),
          lat = as.numeric(.data[["lat"]])
        ) |>
        dplyr::distinct(.data[["dataset_id"]], .keep_all = TRUE)
    }
  ),
  # Why: Restrict the focal SPD to the shared 8--2 ka comparison grid.
  targets::tar_target(
    name = "data_human_proxy_spd_primary",
    prepare_spd_human_proxy_input(
      data_spd = data_human_proxy_spd_focal,
      age_min = min(config_human_proxy[["target_ages"]]),
      age_max = max(config_human_proxy[["target_ages"]]),
      age_step = 500
    )
  ),
  # Why: Create unique record-radius keys for one-pass raster extraction.
  targets::tar_target(
    name = "data_human_proxy_location_keys",
    data_human_proxy_spd_by_radius |>
      dplyr::filter(.data[["available"]]) |>
      dplyr::transmute(
        original_dataset_id = as.character(.data[["dataset_id"]]),
        radius_km = as.numeric(.data[["radius_km"]]),
        dataset_id = stringr::str_c(
          .data[["original_dataset_id"]],
          "__r",
          .data[["radius_km"]]
        )
      ) |>
      dplyr::inner_join(
        data_human_proxy_metadata,
        by = c("original_dataset_id" = "dataset_id"),
        relationship = "many-to-one"
      ) |>
      dplyr::arrange(.data[["dataset_id"]])
  ),
  # Why: Prepare strict 250 and 500 km SPD histories on matched keys.
  targets::tar_target(
    name = "data_human_proxy_spd_radius",
    data_human_proxy_spd_by_radius |>
      dplyr::filter(.data[["available"]]) |>
      dplyr::transmute(
        dataset_id = stringr::str_c(
          as.character(.data[["dataset_id"]]),
          "__r",
          .data[["radius_km"]]
        ),
        distance = as.numeric(.data[["radius_km"]]),
        spd = .data[["spd"]]
      ) |>
      prepare_spd_human_proxy_input(
        age_min = min(config_human_proxy[["target_ages"]]),
        age_max = max(config_human_proxy[["target_ages"]]),
        age_step = 500
      )
  ),
  # Why: Select exact annual KK10 layers on the 500-year target grid.
  targets::tar_target(
    name = "data_human_proxy_layers_kk10",
    build_human_proxy_time_lookup("kk10", 7901L) |>
      dplyr::filter(
        .data[["age_bp"]] %in% config_human_proxy[["target_ages"]]
      ) |>
      dplyr::mutate(
        source_layer_index = .data[["layer_index"]],
        layer_index = dplyr::row_number()
      )
  ),
  # Why: Select HYDE layers that bracket every target age.
  targets::tar_target(
    name = "data_human_proxy_layers_hyde",
    build_human_proxy_time_lookup("hyde_3_2", 75L) |>
      dplyr::filter(
        .data[["age_bp"]] >= min(config_human_proxy[["target_ages"]]) - 1000,
        .data[["age_bp"]] <= max(config_human_proxy[["target_ages"]]) + 1000
      ) |>
      dplyr::mutate(
        source_layer_index = .data[["layer_index"]],
        layer_index = dplyr::row_number()
      )
  ),
  # Why: Cache selected KK10 layers once for all spatial extractions.
  targets::tar_target(
    name = "file_human_proxy_kk10_cache",
    build_kk10_raster_cache(
      file_path = file_human_proxy_kk10,
      layer_indices = data_human_proxy_layers_kk10[["source_layer_index"]],
      cache_path = file.path(path_raster_cache, "kk10_8ka_2ka_500yr.tif")
    ),
    format = "file"
  ),
  # Why: Cache HYDE bracketing layers once for buffer and point extraction.
  targets::tar_target(
    name = "file_human_proxy_hyde_cache",
    build_human_proxy_raster_cache(
      file_path = file_human_proxy_hyde,
      source_id = "hyde_3_2",
      expected_layers = 75L,
      layer_indices = data_human_proxy_layers_hyde[["source_layer_index"]],
      cache_path = file.path(path_raster_cache, "hyde_9ka_1ka.tif")
    ),
    format = "file"
  ),
  # Why: Area-average KK10 over both tested archaeological supports.
  targets::tar_target(
    name = "data_human_proxy_kk10_buffer_all",
    load_human_proxy_raster(
      file_path = file_human_proxy_kk10_cache,
      source_id = "kk10_cache",
      expected_layers = nrow(data_human_proxy_layers_kk10)
    ) |>
      aggregate_human_proxy_raster(
        data_locations = data_human_proxy_location_keys,
        data_layers = data_human_proxy_layers_kk10,
        proxy = "kk10",
        aggregation = "area_weighted_mean",
        extraction = "buffer"
      )
  ),
  # Why: Sum and align HYDE population over both tested supports.
  targets::tar_target(
    name = "data_human_proxy_hyde_buffer_all",
    load_human_proxy_raster(
      file_path = file_human_proxy_hyde_cache,
      source_id = "hyde_3_2_cache",
      expected_layers = nrow(data_human_proxy_layers_hyde)
    ) |>
      aggregate_human_proxy_raster(
        data_locations = data_human_proxy_location_keys,
        data_layers = data_human_proxy_layers_hyde,
        proxy = "hyde",
        aggregation = "cell_center_sum",
        extraction = "buffer"
      ) |>
      interpolate_human_proxy_ages(config_human_proxy[["target_ages"]])
  ),
  # Why: Extract point KK10 values for spatial-aggregation sensitivity.
  targets::tar_target(
    name = "data_human_proxy_kk10_point_all",
    load_human_proxy_raster(
      file_path = file_human_proxy_kk10_cache,
      source_id = "kk10_cache",
      expected_layers = nrow(data_human_proxy_layers_kk10)
    ) |>
      aggregate_human_proxy_raster(
        data_locations = data_human_proxy_location_keys,
        data_layers = data_human_proxy_layers_kk10,
        proxy = "kk10",
        aggregation = "area_weighted_mean",
        extraction = "point"
      )
  ),
  # Why: Extract and align point HYDE values for sensitivity analysis.
  targets::tar_target(
    name = "data_human_proxy_hyde_point_all",
    load_human_proxy_raster(
      file_path = file_human_proxy_hyde_cache,
      source_id = "hyde_3_2_cache",
      expected_layers = nrow(data_human_proxy_layers_hyde)
    ) |>
      aggregate_human_proxy_raster(
        data_locations = data_human_proxy_location_keys,
        data_layers = data_human_proxy_layers_hyde,
        proxy = "hyde",
        aggregation = "cell_center_sum",
        extraction = "point"
      ) |>
      interpolate_human_proxy_ages(config_human_proxy[["target_ages"]])
  ),
  # Why: Map focal records to their selected 250 or 500 km support.
  targets::tar_target(
    name = "data_human_proxy_fallback_keys",
    data_human_proxy_spd_primary |>
      dplyr::distinct(
        original_dataset_id = .data[["dataset_id"]],
        .data[["radius_km"]]
      ) |>
      dplyr::mutate(
        comparison_id = stringr::str_c(
          .data[["original_dataset_id"]],
          "__r",
          .data[["radius_km"]]
        )
      )
  ),
  # Why: Select KK10 values at each focal record support.
  targets::tar_target(
    name = "data_human_proxy_kk10_primary",
    data_human_proxy_kk10_buffer_all |>
      dplyr::inner_join(
        data_human_proxy_fallback_keys,
        by = c("dataset_id" = "comparison_id"),
        relationship = "many-to-one"
      ) |>
      dplyr::mutate(dataset_id = .data[["original_dataset_id"]]) |>
      dplyr::select(-dplyr::all_of("original_dataset_id"))
  ),
  # Why: Select HYDE values at each focal record support.
  targets::tar_target(
    name = "data_human_proxy_hyde_primary",
    data_human_proxy_hyde_buffer_all |>
      dplyr::inner_join(
        data_human_proxy_fallback_keys,
        by = c("dataset_id" = "comparison_id"),
        relationship = "many-to-one"
      ) |>
      dplyr::mutate(dataset_id = .data[["original_dataset_id"]]) |>
      dplyr::select(-dplyr::all_of("original_dataset_id"))
  ),
  # Why: Publish the fully matched and transformed primary observations.
  targets::tar_target(
    name = "data_human_proxy_matches",
    prepare_human_proxy_matches(
      data_spd = data_human_proxy_spd_primary,
      data_kk10 = data_human_proxy_kk10_primary,
      data_hyde = data_human_proxy_hyde_primary,
      data_metadata = data_human_proxy_metadata
    )
  ),
  # Why: Publish recomputable SPD-defined decile summaries.
  targets::tar_target(
    name = "table_human_proxy_decile_summaries",
    summarise_human_proxy_bins(
      data_matched = data_human_proxy_matches,
      bin_count = config_human_proxy[["primary_bins"]]
    )
  ),
  # Why: Estimate primary Kendall correlations and cluster intervals.
  targets::tar_target(
    name = "table_human_proxy_correlations",
    compute_human_proxy_bootstrap_correlations(
      data_matched = data_human_proxy_matches,
      bin_count = config_human_proxy[["primary_bins"]],
      repetitions = config_human_proxy[["bootstrap_repetitions"]],
      seed = config_human_proxy[["bootstrap_seed"]],
      minimum_bins = config_human_proxy[["minimum_bins"]],
      minimum_datasets = config_human_proxy[["minimum_datasets"]],
      minimum_ages = config_human_proxy[["minimum_ages"]]
    )
  ),
  # Why: Test whether results depend on using 5 or 20 SPD bins.
  targets::tar_target(
    name = "table_human_proxy_bin_sensitivity",
    config_human_proxy[["alternative_bins"]] |>
      purrr::map(
        ~ summarise_human_proxy_bins(
          data_human_proxy_matches,
          bin_count = .x
        ) |>
          compute_human_proxy_correlations(
            minimum_bins = .x,
            minimum_datasets = config_human_proxy[["minimum_datasets"]],
            minimum_ages = config_human_proxy[["minimum_ages"]]
          )
      ) |>
      dplyr::bind_rows()
  ),
  # Why: Test convergence after removing broad level trends.
  targets::tar_target(
    name = "table_human_proxy_trend_sensitivity",
    data_human_proxy_matches |>
      prepare_human_proxy_first_differences() |>
      summarise_human_proxy_bins(
        bin_count = config_human_proxy[["primary_bins"]]
      ) |>
      compute_human_proxy_correlations(
        minimum_bins = config_human_proxy[["minimum_bins"]],
        minimum_datasets = config_human_proxy[["minimum_datasets"]],
        minimum_ages = config_human_proxy[["minimum_ages"]]
      )
  ),
  # Why: Match proxy histories separately at strict 250 and 500 km radii.
  targets::tar_target(
    name = "data_human_proxy_matches_radius",
    prepare_human_proxy_matches(
      data_spd = data_human_proxy_spd_radius,
      data_kk10 = data_human_proxy_kk10_buffer_all,
      data_hyde = data_human_proxy_hyde_buffer_all,
      data_metadata = data_human_proxy_location_keys |>
        dplyr::transmute(
          dataset_id = .data[["dataset_id"]],
          region = .data[["region"]],
          long = .data[["long"]],
          lat = .data[["lat"]]
        )
    )
  ),
  # Why: Compare correlations across strict spatial supports.
  targets::tar_target(
    name = "table_human_proxy_radius_sensitivity",
    c(250, 500) |>
      purrr::map(
        ~ data_human_proxy_matches_radius |>
          dplyr::filter(.data[["radius_km"]] == .x) |>
          summarise_human_proxy_bins(
            bin_count = config_human_proxy[["primary_bins"]]
          ) |>
          compute_human_proxy_correlations(
            minimum_bins = config_human_proxy[["minimum_bins"]],
            minimum_datasets = config_human_proxy[["minimum_datasets"]],
            minimum_ages = config_human_proxy[["minimum_ages"]]
          ) |>
          dplyr::mutate(radius_km = .x)
      ) |>
      dplyr::bind_rows()
  ),
  # Why: Test point extraction against record-centred buffers.
  targets::tar_target(
    name = "table_human_proxy_point_sensitivity",
    {
      kk10_point <-
        data_human_proxy_kk10_point_all |>
        dplyr::inner_join(
          data_human_proxy_fallback_keys,
          by = c("dataset_id" = "comparison_id"),
          relationship = "many-to-one"
        ) |>
        dplyr::mutate(dataset_id = .data[["original_dataset_id"]])
      hyde_point <-
        data_human_proxy_hyde_point_all |>
        dplyr::inner_join(
          data_human_proxy_fallback_keys,
          by = c("dataset_id" = "comparison_id"),
          relationship = "many-to-one"
        ) |>
        dplyr::mutate(dataset_id = .data[["original_dataset_id"]])

      prepare_human_proxy_matches(
        data_spd = data_human_proxy_spd_primary,
        data_kk10 = kk10_point,
        data_hyde = hyde_point,
        data_metadata = data_human_proxy_metadata
      ) |>
        summarise_human_proxy_bins(
          bin_count = config_human_proxy[["primary_bins"]]
        ) |>
        compute_human_proxy_correlations(
          minimum_bins = config_human_proxy[["minimum_bins"]],
          minimum_datasets = config_human_proxy[["minimum_datasets"]],
          minimum_ages = config_human_proxy[["minimum_ages"]]
        )
    }
  ),
  # Why: Report matched rows, records, and ages overall and by region.
  targets::tar_target(
    name = "table_human_proxy_coverage",
    dplyr::bind_rows(
      data_human_proxy_matches |>
        dplyr::summarise(
          scope_type = "overall",
          scope = "Overall",
          matched_rows = dplyr::n(),
          matched_datasets = dplyr::n_distinct(.data[["dataset_id"]]),
          matched_ages = dplyr::n_distinct(.data[["age_bp"]])
        ),
      data_human_proxy_matches |>
        dplyr::group_by(scope = .data[["region"]]) |>
        dplyr::summarise(
          scope_type = "region",
          matched_rows = dplyr::n(),
          matched_datasets = dplyr::n_distinct(.data[["dataset_id"]]),
          matched_ages = dplyr::n_distinct(.data[["age_bp"]]),
          .groups = "drop"
        )
    ) |>
      dplyr::select(
        dplyr::all_of(c("scope_type", "scope")),
        dplyr::everything()
      )
  ),
  # Why: Record files, hashes, transformations, thresholds, and seeds.
  targets::tar_target(
    name = "table_human_proxy_provenance",
    table_human_proxy_source_manifest |>
      dplyr::mutate(
        file_md5 = unname(tools::md5sum(.data[["file_path"]])),
        age_min_bp = 2000,
        age_max_bp = 8000,
        age_step_years = 500,
        spd_product = "data_spd_250_with_500_fallback",
        buffer_cell_rule = "cell centre within geodesic buffer",
        spd_transform = "square_root",
        proxy_transform = dplyr::if_else(
          .data[["source_id"]] == "hyde_3_2",
          "square_root",
          "identity"
        ),
        bin_definition = "SPD quantiles with tied cut points retained",
        bootstrap_repetitions = config_human_proxy[["bootstrap_repetitions"]],
        bootstrap_seed = config_human_proxy[["bootstrap_seed"]]
      )
  ),
  # Why: Build the reviewer-facing overall convergence figure.
  targets::tar_target(
    name = "plot_human_proxy_overall",
    plot_human_proxy_convergence(
      data_bins = table_human_proxy_decile_summaries,
      data_correlations = table_human_proxy_correlations,
      scope_type = "overall"
    )
  ),
  # Why: Build regional panels with explicit coverage eligibility.
  targets::tar_target(
    name = "plot_human_proxy_regions",
    plot_human_proxy_convergence(
      data_bins = table_human_proxy_decile_summaries,
      data_correlations = table_human_proxy_correlations,
      scope_type = "region"
    )
  ),
  # Why: Export all numerical evidence for independent recomputation.
  targets::tar_target(
    name = "files_human_proxy_convergence_tables",
    save_human_proxy_convergence_tables(
      data_tables = list(
        matched_values = data_human_proxy_matches,
        decile_summaries = table_human_proxy_decile_summaries,
        correlations = table_human_proxy_correlations,
        coverage = table_human_proxy_coverage,
        provenance = table_human_proxy_provenance,
        bin_sensitivity = table_human_proxy_bin_sensitivity,
        trend_sensitivity = table_human_proxy_trend_sensitivity,
        radius_sensitivity = table_human_proxy_radius_sensitivity,
        point_sensitivity = table_human_proxy_point_sensitivity
      ),
      file_paths = c(
        matched_values = file.path(path_tables, "matched_values.csv.gz"),
        decile_summaries = file.path(path_tables, "decile_summaries.csv"),
        correlations = file.path(path_tables, "correlations.csv"),
        coverage = file.path(path_tables, "coverage.csv"),
        provenance = file.path(path_tables, "provenance.csv"),
        bin_sensitivity = file.path(path_tables, "bin_sensitivity.csv"),
        trend_sensitivity = file.path(path_tables, "trend_sensitivity.csv"),
        radius_sensitivity = file.path(path_tables, "radius_sensitivity.csv"),
        point_sensitivity = file.path(path_tables, "point_sensitivity.csv")
      )
    ),
    format = "file"
  ),
  # Why: Export overall and regional plots in PNG and PDF formats.
  targets::tar_target(
    name = "files_human_proxy_convergence_figures",
    save_human_proxy_convergence_figures(
      plot_overall = plot_human_proxy_overall,
      plot_regions = plot_human_proxy_regions,
      path_overall = file.path(path_figures, "spd_proxy_convergence_overall"),
      path_regions = file.path(path_figures, "spd_proxy_convergence_regions")
    ),
    format = "file"
  )
)
