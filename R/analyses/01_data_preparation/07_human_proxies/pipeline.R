#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#        Human proxies at their native temporal coverage
#
#
#                   O. Mottl, V.A. Felde
#                         2026
#
#----------------------------------------------------------#
# Defines the canonical human-proxy data-layer target graph: SPD, KK10, and
#   HYDE 3.5 at each proxy's native temporal coverage and at several spatial
#   supports around each pollen record.
# Run with:
#   R/analyses/01_data_preparation/00_run.R
# Sourcing this script only declares targets; it does not execute them.
# Raw external rasters are never copied into the repository.

#----------------------------------------------------------#
# 0. Configure pipeline -----
#----------------------------------------------------------#

library(here)

source(here::here("R/00_Config_file.R"))

runner_data_preparation <-
  "R/analyses/01_data_preparation/00_run.R"

store_spd <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "data_preparation/spd"
  )

path_sources <-
  here::here(
    "R",
    "analyses",
    "01_data_preparation",
    "07_human_proxies",
    "proxy_sources.csv"
  )

path_tables <-
  here::here(
    "Outputs",
    "Tables",
    "Data_preparation",
    "Human_proxies"
  )

# Own cache folder, so the caches of the convergence sensitivity in the
#   parent folder are never overwritten.
path_raster_cache <-
  file.path(
    data_storage_path,
    "Human_impact",
    "External_proxies",
    "cache",
    "human_proxies"
  )

config_human_proxies <-
  list(
    support_radii_km = c(10, 25, 50),
    ages_spd_bp = seq(from = 2000, to = 8500, by = 500),
    ages_kk10_bp = seq(from = 500, to = 8000, by = 500),
    ages_hyde_bp = seq(from = 500, to = 11500, by = 500),
    hyde_source_ids = c(
      "hyde_3_5_cropland",
      "hyde_3_5_grazing",
      "hyde_3_5_population_density",
      "hyde_3_5_cell_area"
    ),
    early_holocene_min_age_bp = 6000,
    land_use_threshold = 0.01,
    change_tolerance = 1e-6,
    # NULL keeps every record; a character vector of dataset IDs restricts
    #   the graph to those records (smoke runs only).
    dataset_subset = NULL
  )

#----------------------------------------------------------#
# 1. Define targets -----
#----------------------------------------------------------#

list(
  # Why: Track the SPD radius product so changes to it invalidate the SPD
  #   values and the SPD buffer of every record.
  targets::tar_target(
    name = "fingerprint_proxy_layer_spd",
    command = compute_target_store_fingerprint(
      store = store_spd,
      target_names = "data_spd_by_radius",
      runner = runner_data_preparation
    ),
    cue = targets::tar_cue(mode = "always")
  ),
  # Why: Track record metadata as a file so coordinate, region, or climate
  #   zone changes invalidate every extraction.
  targets::tar_target(
    name = "file_proxy_layer_metadata",
    command = resolve_latest_file_path(
      file_name = "data_meta",
      dir = file.path(data_storage_path, "Assembly")
    ),
    format = "file"
  ),
  # Why: Provide record coordinates, regions, and climate zones once for
  #   extraction, coverage, and regional aggregation.
  targets::tar_target(
    name = "data_proxy_layer_metadata",
    command = resolve_file_path(file_proxy_layer_metadata) |>
      dplyr::mutate(
        dataset_id = as.character(.data[["dataset_id"]]),
        region = as.character(.data[["region"]]),
        climatezone = as.character(.data[["climatezone"]])
      ) |>
      dplyr::select(
        dplyr::all_of(
          c("dataset_id", "long", "lat", "region", "climatezone")
        )
      ) |>
      dplyr::filter(
        is.null(config_human_proxies[["dataset_subset"]]) |
          .data[["dataset_id"]] %in% config_human_proxies[["dataset_subset"]]
      )
  ),
  # Why: Track the checked-in source manifest as a file dependency.
  targets::tar_target(
    name = "file_proxy_layer_source_manifest",
    command = path_sources,
    format = "file"
  ),
  # Why: Validate source metadata and sizes and fail clearly when external
  #   rasters are absent.
  targets::tar_target(
    name = "table_proxy_layer_source_manifest",
    command = readr::read_csv(
      file_proxy_layer_source_manifest,
      show_col_types = FALSE
    ) |>
      validate_human_proxy_source_manifest(
        data_storage_path = data_storage_path,
        require_files = TRUE,
        expected_source_ids = c(
          "kk10",
          config_human_proxies[["hyde_source_ids"]]
        ),
        acquisition_hint = stringr::str_c(
          "See R/analyses/01_data_preparation/07_human_proxies/README.md ",
          "for where to download each source."
        )
      )
  ),
  # Why: Track the large KK10 NetCDF without copying it into the repository.
  targets::tar_target(
    name = "file_proxy_layer_kk10",
    command = table_proxy_layer_source_manifest |>
      dplyr::filter(.data[["source_id"]] == "kk10") |>
      dplyr::pull("file_path"),
    format = "file"
  ),
  # Why: Track the HYDE 3.5 cropland NetCDF as a file dependency.
  targets::tar_target(
    name = "file_proxy_layer_hyde_cropland",
    command = table_proxy_layer_source_manifest |>
      dplyr::filter(.data[["source_id"]] == "hyde_3_5_cropland") |>
      dplyr::pull("file_path"),
    format = "file"
  ),
  # Why: Track the HYDE 3.5 grazing-land NetCDF as a file dependency.
  targets::tar_target(
    name = "file_proxy_layer_hyde_grazing",
    command = table_proxy_layer_source_manifest |>
      dplyr::filter(.data[["source_id"]] == "hyde_3_5_grazing") |>
      dplyr::pull("file_path"),
    format = "file"
  ),
  # Why: Track the HYDE 3.5 population-density NetCDF as a file dependency.
  targets::tar_target(
    name = "file_proxy_layer_hyde_population",
    command = table_proxy_layer_source_manifest |>
      dplyr::filter(
        .data[["source_id"]] == "hyde_3_5_population_density"
      ) |>
      dplyr::pull("file_path"),
    format = "file"
  ),
  # Why: Track the HYDE 3.5 cell-area grid, the land-use denominator, as a
  #   file dependency.
  targets::tar_target(
    name = "file_proxy_layer_hyde_cell_area",
    command = table_proxy_layer_source_manifest |>
      dplyr::filter(.data[["source_id"]] == "hyde_3_5_cell_area") |>
      dplyr::pull("file_path"),
    format = "file"
  ),
  # Why: Record observed sizes and checksums of every external source for
  #   the provenance table.
  targets::tar_target(
    name = "table_proxy_layer_file_checksums",
    command = compute_human_proxy_file_checksums(
      table_proxy_layer_source_manifest
    )
  ),
  # Why: Derive the 250 km SPD with 500 km fallback from the radius product
  #   with the SPD pipeline's own policy function, so stores built before the
  #   public fallback target stay usable. It gives both the SPD values and
  #   each record's SPD buffer.
  targets::tar_target(
    name = "data_proxy_layer_spd_fallback",
    command = {
      fingerprint_proxy_layer_spd

      load_target_store_value(
        store = store_spd,
        target_name = "data_spd_by_radius",
        runner = runner_data_preparation
      ) |>
        prepare_spd_analysis_products() |>
        purrr::chuck("data_spd_250_with_500_fallback") |>
        dplyr::mutate(
          dataset_id = as.character(.data[["dataset_id"]])
        ) |>
        dplyr::filter(
          .data[["dataset_id"]] %in%
            dplyr::pull(data_proxy_layer_metadata, "dataset_id")
        )
    }
  ),
  # Why: Define every record x support extraction location once so all
  #   gridded proxies share identical supports.
  targets::tar_target(
    name = "data_proxy_layer_supports",
    command = prepare_human_proxy_support_locations(
      data_meta = data_proxy_layer_metadata,
      data_spd_support = data_proxy_layer_spd_fallback |>
        dplyr::select(
          dataset_id = "dataset_id",
          radius_km = "distance"
        ),
      radii_km = config_human_proxies[["support_radii_km"]]
    )
  ),
  # Why: Publish SPD at its canonical analysis window on the SPD buffer,
  #   square-root transformed as in every model run.
  targets::tar_target(
    name = "data_proxy_layer_spd",
    command = prepare_spd_human_proxy_input(
      data_spd = data_proxy_layer_spd_fallback,
      age_min = min(config_human_proxies[["ages_spd_bp"]]),
      age_max = max(config_human_proxies[["ages_spd_bp"]]),
      age_step = 500
    ) |>
      dplyr::mutate(
        support = "spd_buffer",
        value = .data[["spd"]]
      ) |>
      prepare_human_proxy_values(
        proxy = "spd",
        native_ages_bp = seq(from = min_age, to = max_age, by = 1),
        transformation = "square_root"
      )
  ),
  # Why: Select the exact annual KK10 layers on the 500-year age grid.
  targets::tar_target(
    name = "data_proxy_layer_kk10_layers",
    command = build_human_proxy_time_lookup(
      source_id = "kk10",
      n_layers = 7901L
    ) |>
      dplyr::filter(
        .data[["age_bp"]] %in% config_human_proxies[["ages_kk10_bp"]]
      ) |>
      dplyr::mutate(
        source_layer_index = .data[["layer_index"]],
        layer_index = dplyr::row_number()
      )
  ),
  # Why: Cache the selected KK10 layers once for all spatial extractions.
  targets::tar_target(
    name = "file_proxy_layer_kk10_cache",
    command = build_kk10_raster_cache(
      file_path = file_proxy_layer_kk10,
      layer_indices = dplyr::pull(
        data_proxy_layer_kk10_layers,
        "source_layer_index"
      ),
      cache_path = file.path(
        path_raster_cache,
        "kk10_8000_500bp_500yr.tif"
      )
    ),
    format = "file"
  ),
  # Why: Extract KK10 at the record cell, the small radii, and the SPD
  #   buffer of every record.
  targets::tar_target(
    name = "data_proxy_layer_kk10_extracted",
    command = load_human_proxy_raster(
      file_path = file_proxy_layer_kk10_cache,
      source_id = "kk10_cache",
      expected_layers = nrow(data_proxy_layer_kk10_layers)
    ) |>
      aggregate_human_proxy_supports(
        data_support_locations = data_proxy_layer_supports,
        data_layers = data_proxy_layer_kk10_layers,
        proxy = "kk10",
        aggregation = "area_weighted_mean"
      )
  ),
  # Why: Standardise KK10 to the public contract; every value is an exact
  #   annual layer.
  targets::tar_target(
    name = "data_proxy_layer_kk10",
    command = prepare_human_proxy_values(
      data_source = data_proxy_layer_kk10_extracted,
      proxy = "kk10",
      native_ages_bp = build_human_proxy_time_lookup(
        source_id = "kk10",
        n_layers = 7901L
      ) |>
        dplyr::pull("age_bp"),
      transformation = "identity"
    )
  ),
  # Why: Read the HYDE 3.5 time axis once and check that the three gridded
  #   variables share the same time steps.
  targets::tar_target(
    name = "data_proxy_layer_hyde_time_lookup",
    command = load_hyde_time_lookup(
      file_paths = c(
        file_proxy_layer_hyde_cropland,
        file_proxy_layer_hyde_grazing,
        file_proxy_layer_hyde_population
      ),
      source_id = "hyde_3_5"
    )
  ),
  # Why: Keep only the HYDE layers that bracket the 500-year target ages, so
  #   values are interpolated and never extrapolated.
  targets::tar_target(
    name = "data_proxy_layer_hyde_layers",
    command = select_human_proxy_bracketing_layers(
      data_lookup = data_proxy_layer_hyde_time_lookup,
      target_ages_bp = config_human_proxies[["ages_hyde_bp"]]
    )
  ),
  # Why: Cache the HYDE land-use fraction of the bracketing layers once for
  #   all spatial extractions.
  targets::tar_target(
    name = "file_proxy_layer_hyde_land_use_cache",
    command = save_hyde_land_use_fraction_cache(
      path_cropland = file_proxy_layer_hyde_cropland,
      path_grazing = file_proxy_layer_hyde_grazing,
      path_cell_area = file_proxy_layer_hyde_cell_area,
      layer_indices = dplyr::pull(
        data_proxy_layer_hyde_layers,
        "source_layer_index"
      ),
      cache_path = file.path(
        path_raster_cache,
        "hyde_3_5_land_use_fraction.tif"
      )
    ),
    format = "file"
  ),
  # Why: Cache the HYDE population density of the bracketing layers once
  #   for all spatial extractions.
  targets::tar_target(
    name = "file_proxy_layer_hyde_population_cache",
    command = build_human_proxy_raster_cache(
      file_path = file_proxy_layer_hyde_population,
      source_id = "hyde_3_5_population_density",
      expected_layers = nrow(data_proxy_layer_hyde_time_lookup),
      layer_indices = dplyr::pull(
        data_proxy_layer_hyde_layers,
        "source_layer_index"
      ),
      cache_path = file.path(
        path_raster_cache,
        "hyde_3_5_population_density.tif"
      )
    ),
    format = "file"
  ),
  # Why: Extract HYDE land use at every support on the bracketing layers.
  targets::tar_target(
    name = "data_proxy_layer_hyde_land_use_extracted",
    command = load_human_proxy_raster(
      file_path = file_proxy_layer_hyde_land_use_cache,
      source_id = "hyde_3_5_land_use_cache",
      expected_layers = nrow(data_proxy_layer_hyde_layers)
    ) |>
      aggregate_human_proxy_supports(
        data_support_locations = data_proxy_layer_supports,
        data_layers = data_proxy_layer_hyde_layers,
        proxy = "hyde_3_5_land_use",
        aggregation = "area_weighted_mean"
      )
  ),
  # Why: Extract HYDE population density at every support on the
  #   bracketing layers.
  targets::tar_target(
    name = "data_proxy_layer_hyde_population_extracted",
    command = load_human_proxy_raster(
      file_path = file_proxy_layer_hyde_population_cache,
      source_id = "hyde_3_5_population_cache",
      expected_layers = nrow(data_proxy_layer_hyde_layers)
    ) |>
      aggregate_human_proxy_supports(
        data_support_locations = data_proxy_layer_supports,
        data_layers = data_proxy_layer_hyde_layers,
        proxy = "hyde_3_5_population",
        aggregation = "area_weighted_mean"
      )
  ),
  # Why: Interpolate HYDE land use to the 500-year ages and standardise it
  #   to the public contract.
  targets::tar_target(
    name = "data_proxy_layer_hyde_land_use",
    command = interpolate_human_proxy_support_ages(
      data_extracted = data_proxy_layer_hyde_land_use_extracted,
      target_ages_bp = config_human_proxies[["ages_hyde_bp"]]
    ) |>
      prepare_human_proxy_values(
        proxy = "hyde_3_5_land_use",
        native_ages_bp = dplyr::pull(
          data_proxy_layer_hyde_time_lookup,
          "age_bp"
        ),
        transformation = "identity"
      )
  ),
  # Why: Interpolate HYDE population density to the 500-year ages and
  #   standardise it, square-root transformed, to the public contract.
  targets::tar_target(
    name = "data_proxy_layer_hyde_population",
    command = interpolate_human_proxy_support_ages(
      data_extracted = data_proxy_layer_hyde_population_extracted,
      target_ages_bp = config_human_proxies[["ages_hyde_bp"]]
    ) |>
      prepare_human_proxy_values(
        proxy = "hyde_3_5_population",
        native_ages_bp = dplyr::pull(
          data_proxy_layer_hyde_time_lookup,
          "age_bp"
        ),
        transformation = "square_root"
      )
  ),
  # Why: Declare each proxy's age window, supports, aggregation, and
  #   transformation once for validation and provenance.
  targets::tar_target(
    name = "table_human_proxy_layer_settings",
    command = tibble::tibble(
      proxy = c(
        "spd",
        "kk10",
        "hyde_3_5_land_use",
        "hyde_3_5_population"
      ),
      source = c(
        "data_preparation/spd: data_spd_by_radius, 250 km with 500 km fallback",
        "kk10",
        "hyde_3_5_cropland + hyde_3_5_grazing / hyde_3_5_cell_area",
        "hyde_3_5_population_density"
      ),
      variable = c(
        "summed probability of calibrated radiocarbon dates (not normalised)",
        "fraction of cell under land use",
        "fraction of cell under cropland or grazing land",
        "people per km2"
      ),
      age_min_bp = c(
        min(config_human_proxies[["ages_spd_bp"]]),
        min(config_human_proxies[["ages_kk10_bp"]]),
        min(config_human_proxies[["ages_hyde_bp"]]),
        min(config_human_proxies[["ages_hyde_bp"]])
      ),
      age_max_bp = c(
        max(config_human_proxies[["ages_spd_bp"]]),
        max(config_human_proxies[["ages_kk10_bp"]]),
        max(config_human_proxies[["ages_hyde_bp"]]),
        max(config_human_proxies[["ages_hyde_bp"]])
      ),
      age_step_years = 500,
      supports = c(
        "spd_buffer",
        rep(
          dplyr::pull(data_proxy_layer_supports, "support") |>
            unique() |>
            stringr::str_c(collapse = ";"),
          3
        )
      ),
      spatial_aggregation = c(
        "SPD of radiocarbon dates within 250 km, or 500 km when fewer than 50",
        rep(
          stringr::str_c(
            "cell: value of the containing cell; buffers: area-weighted mean ",
            "of cells whose centre lies within the geodesic buffer"
          ),
          3
        )
      ),
      time_alignment = c(
        "annual series read at exact ages",
        "exact annual layers",
        rep(
          stringr::str_c(
            "linear interpolation between the two bracketing HYDE layers; ",
            "never extrapolated"
          ),
          2
        )
      ),
      transformation = c("square_root", "identity", "identity", "square_root")
    )
  ),
  # Why: Publish one validated long table of every proxy, record, age, and
  #   support for all downstream single-proxy analyses.
  targets::tar_target(
    name = "data_human_proxy_values",
    command = dplyr::bind_rows(
      data_proxy_layer_spd,
      data_proxy_layer_kk10,
      data_proxy_layer_hyde_land_use,
      data_proxy_layer_hyde_population
    ) |>
      validate_human_proxy_values(
        data_proxy_windows = table_human_proxy_layer_settings |>
          dplyr::select(
            dplyr::all_of(c("proxy", "age_min_bp", "age_max_bp"))
          )
      )
  ),
  # Why: Publish proxies averaged by continental region x climate zone x
  #   age for the H2 group-level analyses.
  targets::tar_target(
    name = "data_human_proxy_regional",
    command = aggregate_human_proxy_regional(
      data_values = data_human_proxy_values,
      data_meta = data_proxy_layer_metadata
    )
  ),
  # Why: Report record coverage and native against interpolated values by
  #   proxy, support, region, and age.
  targets::tar_target(
    name = "table_human_proxy_layer_coverage",
    command = summarise_human_proxy_coverage(
      data_values = data_human_proxy_values,
      data_meta = data_proxy_layer_metadata
    )
  ),
  # Why: Summarise variation among records and through time to judge where
  #   each proxy carries information.
  targets::tar_target(
    name = "table_human_proxy_age_variation",
    command = summarise_human_proxy_age_variation(
      data_values = data_human_proxy_values,
      value_threshold = config_human_proxies[["land_use_threshold"]],
      change_tolerance = config_human_proxies[["change_tolerance"]]
    )
  ),
  # Why: Isolate the early-Holocene land-use check that decides whether
  #   the native HYDE 3.5 window becomes a main result.
  targets::tar_target(
    name = "table_human_proxy_early_holocene_check",
    command = table_human_proxy_age_variation |>
      dplyr::filter(
        .data[["proxy"]] %in% c("hyde_3_5_land_use", "kk10"),
        .data[["age_bp"]] >=
          config_human_proxies[["early_holocene_min_age_bp"]]
      )
  ),
  # Why: Record source files, checksums, versions, and the source time
  #   layers used for every proxy.
  targets::tar_target(
    name = "table_human_proxy_layer_provenance",
    command = build_human_proxy_source_provenance(
      data_sources = table_proxy_layer_source_manifest,
      data_checksums = table_proxy_layer_file_checksums,
      data_layers = dplyr::bind_rows(
        data_proxy_layer_kk10_layers |>
          dplyr::select(dplyr::all_of(c("source_id", "age_bp"))),
        tidyr::crossing(
          source_id = c(
            "hyde_3_5_cropland",
            "hyde_3_5_grazing",
            "hyde_3_5_population_density"
          ),
          age_bp = dplyr::pull(data_proxy_layer_hyde_layers, "age_bp")
        )
      )
    )
  ),
  # Why: Export coverage, provenance, settings, and the variation checks as
  #   tracked tables for review and reporting.
  targets::tar_target(
    name = "files_human_proxy_layer_tables",
    command = save_human_proxy_convergence_tables(
      data_tables = list(
        coverage = table_human_proxy_layer_coverage,
        provenance = table_human_proxy_layer_provenance,
        settings = table_human_proxy_layer_settings,
        age_variation = table_human_proxy_age_variation,
        early_holocene_check = table_human_proxy_early_holocene_check
      ),
      file_paths = c(
        coverage = file.path(path_tables, "human_proxies__coverage.csv"),
        provenance = file.path(path_tables, "human_proxies__provenance.csv"),
        settings = file.path(path_tables, "human_proxies__settings.csv"),
        age_variation = file.path(
          path_tables,
          "human_proxies__age_variation.csv"
        ),
        early_holocene_check = file.path(
          path_tables,
          "human_proxies__early_holocene_check.csv"
        )
      )
    ),
    format = "file"
  )
)
