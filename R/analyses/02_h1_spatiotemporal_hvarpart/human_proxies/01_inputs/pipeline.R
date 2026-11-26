#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#            Single-proxy H1 inputs: record and
#        region-by-age units with a shared climate selection
#
#                   O. Mottl, V.A. Felde
#                         2026
#
#----------------------------------------------------------#
# Defines the inputs of the single-proxy H1 runs.
# Run with:
#   R/analyses/02_h1_spatiotemporal_hvarpart/human_proxies/00_run.R
# Sourcing this script only declares targets; it does not execute them.
# Reads the uncut PAPs and predictors (0-12 ka) and the human-proxy data
#   layer; the PAP, predictor, and data-layer stores are read only.

#----------------------------------------------------------#
# 0. Configure pipeline -----
#----------------------------------------------------------#

library(here)

source(here::here("R/00_Config_file.R"))

runner_data_preparation <-
  "R/analyses/01_data_preparation/00_run.R"

store_paps <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "data_preparation/paps"
  )

store_predictors <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "data_preparation/predictors"
  )

store_human_proxies <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "data_preparation/human_proxies"
  )

store_spd <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "data_preparation/spd"
  )

path_profiles <-
  here::here("R", "analyses", "00_profiles", "analysis_profiles.csv")

path_tables <-
  here::here("Outputs", "Tables", "H1", "Proxies", "Inputs")

config_h1_human_proxies <-
  list(
    configuration_reference = "human_proxy_h1",
    common_window_bp = c(2000, 8000),
    events_window_bp = c(500, 11500),
    common_slice_set = "common_cohort_2000_8000",
    native_slice_set = "all_records_500_11500",
    # Region-by-age units of the native runs come from this window, which
    #   must cover every native window.
    native_slice_window_id = "native_500_11500",
    response_vars = c(
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
    ),
    climate_candidates = c(
      "temp_annual",
      "temp_cold",
      "prec_summer",
      "prec_win"
    ),
    event_columns = c(
      "fi",
      "fc",
      "ec",
      "cc",
      "es",
      "ei",
      "weak",
      "medium",
      "strong"
    ),
    max_cor = 0.8,
    max_vif = 5,
    # Proxy and support combinations every common-cohort record must have:
    #   the three main proxies at the SPD buffer and the two gridded proxies
    #   at 50 km, so main and 50 km runs share identical records.
    common_cohort_columns = c(
      "spd__spd_buffer",
      "kk10__spd_buffer",
      "hyde_3_5_land_use__spd_buffer",
      "kk10__radius_50km",
      "hyde_3_5_land_use__radius_50km"
    ),
    seed = 1234L,
    permutations = 999L,
    alpha = 0.05,
    spatial_distances_km = c(250, 500),
    temporal_distances_years = c(500, 1000),
    min_unique_locations = 20L,
    min_spatial_residual_df = 10L,
    min_unique_ages = 10L,
    min_temporal_residual_df = 5L,
    n_bootstrap = 1999L,
    temporal_block_length = 3L
  )

#----------------------------------------------------------#
# 1. Define targets -----
#----------------------------------------------------------#

list(
  # Why: Track the profile registry so new or changed single-proxy profiles
  #   invalidate the run specifications.
  targets::tar_target(
    name = "file_h1_proxy_profiles",
    command = path_profiles,
    format = "file"
  ),
  # Why: Keep the validated single-proxy H1 profiles that define every run.
  targets::tar_target(
    name = "data_h1_proxy_profiles",
    command = load_analysis_profiles(file_h1_proxy_profiles) |>
      dplyr::filter(
        .data[["configuration_reference"]] ==
          config_h1_human_proxies[["configuration_reference"]],
        .data[["enabled"]]
      )
  ),
  # Why: Publish the analysis settings once so every run uses the same seed,
  #   thresholds, and bootstrap settings.
  targets::tar_target(
    name = "h1_proxy_config",
    command = config_h1_human_proxies
  ),
  # Why: Publish the response columns once for every run.
  targets::tar_target(
    name = "h1_proxy_response_variables",
    command = config_h1_human_proxies[["response_vars"]]
  ),
  # Why: Fingerprint the uncut PAPs so upstream changes invalidate this store.
  targets::tar_target(
    name = "fingerprint_h1_proxy_paps",
    command = compute_target_store_fingerprint(
      store = store_paps,
      target_names = "data_properties",
      runner = runner_data_preparation
    ),
    cue = targets::tar_cue(mode = "always")
  ),
  # Why: Fingerprint the uncut predictors so upstream changes invalidate this
  #   store.
  targets::tar_target(
    name = "fingerprint_h1_proxy_predictors",
    command = compute_target_store_fingerprint(
      store = store_predictors,
      target_names = "data_predictors",
      runner = runner_data_preparation
    ),
    cue = targets::tar_cue(mode = "always")
  ),
  # Why: Fingerprint the human-proxy data layer so new proxy values
  #   invalidate this store.
  targets::tar_target(
    name = "fingerprint_h1_proxy_layer",
    command = compute_target_store_fingerprint(
      store = store_human_proxies,
      target_names = c(
        "data_human_proxy_values",
        "table_human_proxy_layer_settings"
      ),
      runner = runner_data_preparation
    ),
    cue = targets::tar_cue(mode = "always")
  ),
  # Why: Fingerprint the SPD radius product, which identifies records with no
  #   SPD at either radius.
  targets::tar_target(
    name = "fingerprint_h1_proxy_spd",
    command = compute_target_store_fingerprint(
      store = store_spd,
      target_names = "data_spd_by_radius",
      runner = runner_data_preparation
    ),
    cue = targets::tar_cue(mode = "always")
  ),
  # Why: Import the uncut PAPs (0-12 ka) needed by the native windows.
  targets::tar_target(
    name = "data_h1_proxy_properties",
    command = {
      fingerprint_h1_proxy_paps

      load_target_store_value(
        store = store_paps,
        target_name = "data_properties",
        runner = runner_data_preparation
      )
    }
  ),
  # Why: Import the uncut climate and event predictors (0-12 ka).
  targets::tar_target(
    name = "data_h1_proxy_predictors",
    command = {
      fingerprint_h1_proxy_predictors

      load_target_store_value(
        store = store_predictors,
        target_name = "data_predictors",
        runner = runner_data_preparation
      )
    }
  ),
  # Why: Import every human proxy at its native coverage and support.
  targets::tar_target(
    name = "data_h1_proxy_layer_values",
    command = {
      fingerprint_h1_proxy_layer

      load_target_store_value(
        store = store_human_proxies,
        target_name = "data_human_proxy_values",
        runner = runner_data_preparation
      )
    }
  ),
  # Why: Import each proxy's native window from the data layer, so the run
  #   windows follow the layer rather than a second copy of the ages.
  targets::tar_target(
    name = "table_h1_proxy_layer_settings",
    command = {
      fingerprint_h1_proxy_layer

      load_target_store_value(
        store = store_human_proxies,
        target_name = "table_human_proxy_layer_settings",
        runner = runner_data_preparation
      )
    }
  ),
  # Why: List records with no SPD at either radius; their stored SPD of zero
  #   is a fill value, so they are excluded from SPD runs and the cohort.
  targets::tar_target(
    name = "data_h1_proxy_spd_unavailable",
    command = {
      fingerprint_h1_proxy_spd

      load_target_store_value(
        store = store_spd,
        target_name = "data_spd_by_radius",
        runner = runner_data_preparation
      ) |>
        dplyr::summarise(
          is_available = any(.data[["available"]]),
          .by = "dataset_id"
        ) |>
        dplyr::filter(!.data[["is_available"]]) |>
        dplyr::transmute(
          dataset_id = as.character(.data[["dataset_id"]]),
          proxy = "spd"
        )
    }
  ),
  # Why: Track the record metadata file used for regions and coordinates.
  targets::tar_target(
    name = "file_h1_proxy_meta",
    command = resolve_latest_file_path(
      file_name = "data_meta",
      dir = file.path(data_storage_path, "Assembly")
    ),
    format = "file"
  ),
  # Why: Provide record regions, climate zones, coordinates, and publicity
  #   with character identifiers that match the proxy layer.
  targets::tar_target(
    name = "data_h1_proxy_meta",
    command = resolve_file_path(file_h1_proxy_meta) |>
      dplyr::mutate(dataset_id = as.character(.data[["dataset_id"]]))
  ),
  # Why: Define the common-cohort window and each proxy's native window.
  targets::tar_target(
    name = "data_h1_proxy_windows",
    command = build_human_proxy_h1_windows(
      data_proxy_windows = table_h1_proxy_layer_settings |>
        dplyr::select(
          dplyr::all_of(c("proxy", "age_min_bp", "age_max_bp"))
        ),
      common_window_bp = config_h1_human_proxies[["common_window_bp"]],
      events_window_bp = config_h1_human_proxies[["events_window_bp"]]
    )
  ),
  # Why: Translate every single-proxy profile into its human block, window,
  #   and support.
  targets::tar_target(
    name = "data_h1_proxy_specifications",
    command = build_human_proxy_h1_specifications(
      data_profiles = data_h1_proxy_profiles,
      data_windows = data_h1_proxy_windows,
      event_columns = config_h1_human_proxies[["event_columns"]]
    )
  ),
  # Why: Widen the proxy layer to one column per proxy and support used by a
  #   run or by the common-cohort definition.
  targets::tar_target(
    name = "data_h1_proxy_columns",
    command = prepare_human_proxy_predictor_columns(
      data_proxy_values = data_h1_proxy_layer_values,
      data_column_specs = data_h1_proxy_specifications |>
        dplyr::filter(.data[["human_proxy"]] != "events") |>
        dplyr::transmute(
          proxy = .data[["human_proxy"]],
          support = .data[["proxy_support"]]
        ) |>
        dplyr::bind_rows(
          tibble::tibble(
            column_name =
              config_h1_human_proxies[["common_cohort_columns"]]
          ) |>
            tidyr::separate_wider_delim(
              cols = "column_name",
              delim = "__",
              names = c("proxy", "support")
            )
        ) |>
        dplyr::distinct(),
      data_unavailable = data_h1_proxy_spd_unavailable
    )
  ),
  # Why: Build one history per record and window with responses, climate,
  #   events, and proxies, filtered and age-collapsed as in the H1 inputs.
  targets::tar_target(
    name = "output_h1_proxy_record_units",
    command = prepare_human_proxy_h1_record_units(
      data_properties = data_h1_proxy_properties,
      data_predictors = data_h1_proxy_predictors,
      data_proxy_columns = data_h1_proxy_columns,
      data_meta = data_h1_proxy_meta,
      data_windows = data_h1_proxy_windows |>
        dplyr::distinct(
          .data[["window_id"]],
          .data[["age_min_bp"]],
          .data[["age_max_bp"]]
        ),
      response_vars = config_h1_human_proxies[["response_vars"]],
      predictor_vars = c(
        config_h1_human_proxies[["climate_candidates"]],
        config_h1_human_proxies[["event_columns"]]
      )
    )
  ),
  # Why: Define the common cohort as records complete for every main proxy
  #   at both supports over the common window.
  targets::tar_target(
    name = "output_h1_proxy_common_cohort",
    command = select_human_proxy_common_cohort(
      data_record_units = output_h1_proxy_record_units[["data"]] |>
        dplyr::filter(
          .data[["window_id"]] %in%
            dplyr::pull(
              dplyr::filter(
                data_h1_proxy_windows,
                .data[["age_window"]] == "common_cohort"
              ),
              "window_id"
            )
        ),
      required_columns = config_h1_human_proxies[["common_cohort_columns"]]
    )
  ),
  # Why: Publish the common-cohort records shared by every common-cohort run.
  targets::tar_target(
    name = "data_h1_proxy_common_cohort",
    command = output_h1_proxy_common_cohort[["cohort"]]
  ),
  # Why: Publish the record units of every run: the common window holds only
  #   the common cohort, native windows hold every record.
  targets::tar_target(
    name = "data_h1_proxy_record_units",
    command = output_h1_proxy_record_units[["data"]] |>
      dplyr::filter(
        !stringr::str_starts(.data[["window_id"]], "common_cohort") |
          .data[["dataset_id"]] %in%
            dplyr::pull(data_h1_proxy_common_cohort, "dataset_id")
      )
  ),
  # Why: Stop early if the window feeding the native region-by-age units does
  #   not cover every native window.
  targets::tar_target(
    name = "check_h1_proxy_native_slice_window",
    command = data_h1_proxy_windows |>
      dplyr::filter(.data[["age_window"]] == "native") |>
      dplyr::summarise(
        is_covered =
          min(.data[["age_min_bp"]]) >=
            min(.data[["age_min_bp"]][
              .data[["window_id"]] ==
                config_h1_human_proxies[["native_slice_window_id"]]
            ]) &&
            max(.data[["age_max_bp"]]) <=
              max(.data[["age_max_bp"]][
                .data[["window_id"]] ==
                  config_h1_human_proxies[["native_slice_window_id"]]
              ])
      ) |>
      dplyr::pull("is_covered") |>
      assertthat::assert_that(
        msg = "The native slice window does not cover every native window."
      )
  ),
  # Why: Build region-by-age units once for the common cohort and once for
  #   all records, so every proxy in a unit sees the same records.
  targets::tar_target(
    name = "data_h1_proxy_slice_units",
    command = dplyr::bind_rows(
      prepare_hvarpart_timebin_data(
        data_source = data_h1_proxy_record_units |>
          dplyr::filter(
            stringr::str_starts(.data[["window_id"]], "common_cohort")
          ) |>
          dplyr::select(dplyr::all_of(c("dataset_id", "data_merge"))),
        data_meta = data_h1_proxy_meta
      ) |>
        dplyr::mutate(
          slice_set = config_h1_human_proxies[["common_slice_set"]]
        ),
      prepare_hvarpart_timebin_data(
        data_source = data_h1_proxy_record_units |>
          dplyr::filter(
            .data[["window_id"]] ==
              config_h1_human_proxies[["native_slice_window_id"]],
            check_h1_proxy_native_slice_window
          ) |>
          dplyr::select(dplyr::all_of(c("dataset_id", "data_merge"))),
        data_meta = data_h1_proxy_meta
      ) |>
        dplyr::mutate(
          slice_set = config_h1_human_proxies[["native_slice_set"]]
        )
    ) |>
      dplyr::relocate(dplyr::all_of("slice_set")) |>
      dplyr::arrange(
        .data[["slice_set"]],
        .data[["region"]],
        .data[["age"]]
      )
  ),
  # Why: Select climate once per record and window without the response, so
  #   every proxy fitted to a record reuses the same climate block.
  targets::tar_target(
    name = "data_h1_proxy_record_climate",
    command = select_shared_climate_predictors(
      data_units = data_h1_proxy_record_units,
      unit_columns = c("window_id", "dataset_id"),
      climate_candidates = config_h1_human_proxies[["climate_candidates"]],
      max_cor = config_h1_human_proxies[["max_cor"]],
      max_vif = config_h1_human_proxies[["max_vif"]]
    )
  ),
  # Why: Select climate once per region-by-age unit without the response.
  targets::tar_target(
    name = "data_h1_proxy_slice_climate",
    command = select_shared_climate_predictors(
      data_units = data_h1_proxy_slice_units,
      unit_columns = c("slice_set", "region", "age"),
      climate_candidates = config_h1_human_proxies[["climate_candidates"]],
      max_cor = config_h1_human_proxies[["max_cor"]],
      max_vif = config_h1_human_proxies[["max_vif"]]
    )
  ),
  # Why: Report how often each climate variable is retained per record, by
  #   region and window.
  targets::tar_target(
    name = "table_h1_proxy_climate_retention_records",
    command = summarise_climate_retention_frequency(
      data_climate_selection = data_h1_proxy_record_climate |>
        dplyr::left_join(
          data_h1_proxy_meta |>
            dplyr::select(dplyr::all_of(c("dataset_id", "region"))),
          by = "dataset_id"
        ),
      climate_candidates = config_h1_human_proxies[["climate_candidates"]],
      group_columns = "window_id"
    )
  ),
  # Why: Report how often each climate variable is retained per region-by-age
  #   unit.
  targets::tar_target(
    name = "table_h1_proxy_climate_retention_slices",
    command = summarise_climate_retention_frequency(
      data_climate_selection = data_h1_proxy_slice_climate,
      climate_candidates = config_h1_human_proxies[["climate_candidates"]],
      group_columns = "slice_set"
    )
  ),
  # Why: Record the settings and upstream fingerprints behind every input.
  targets::tar_target(
    name = "table_h1_proxy_input_provenance",
    command = tibble::tibble(
      field = c(
        "common_window_bp",
        "events_window_bp",
        "climate_candidates",
        "climate_filter",
        "common_cohort_columns",
        "records_without_spd",
        "fingerprint_paps",
        "fingerprint_predictors",
        "fingerprint_human_proxy_layer",
        "fingerprint_spd"
      ),
      value = c(
        stringr::str_c(
          config_h1_human_proxies[["common_window_bp"]],
          collapse = "-"
        ),
        stringr::str_c(
          config_h1_human_proxies[["events_window_bp"]],
          collapse = "-"
        ),
        stringr::str_c(
          config_h1_human_proxies[["climate_candidates"]],
          collapse = ";"
        ),
        stringr::str_c(
          "max_cor=", config_h1_human_proxies[["max_cor"]],
          ";max_vif=", config_h1_human_proxies[["max_vif"]]
        ),
        stringr::str_c(
          config_h1_human_proxies[["common_cohort_columns"]],
          collapse = ";"
        ),
        as.character(nrow(data_h1_proxy_spd_unavailable)),
        fingerprint_h1_proxy_paps,
        fingerprint_h1_proxy_predictors,
        fingerprint_h1_proxy_layer,
        fingerprint_h1_proxy_spd
      )
    )
  ),
  # Why: Export cohort, windows, specifications, unit coverage, and the
  #   shared climate selections under stable file names.
  targets::tar_target(
    name = "files_h1_proxy_input_tables",
    command = {
      list_tables <-
        list(
          human_proxies__windows = data_h1_proxy_windows,
          human_proxies__run_specifications =
            data_h1_proxy_specifications,
          human_proxies__common_cohort__records =
            output_h1_proxy_common_cohort[["audit"]],
          human_proxies__record_units__coverage =
            data_h1_proxy_record_units |>
              dplyr::mutate(
                n_ages = purrr::map_int(.data[["data_merge"]], nrow)
              ) |>
              dplyr::summarise(
                n_records = dplyr::n(),
                n_rows = sum(.data[["n_ages"]]),
                .by = "window_id"
              ),
          human_proxies__slice_units__coverage =
            data_h1_proxy_slice_units |>
              dplyr::select(
                dplyr::all_of(c("slice_set", "region", "age", "n_samples"))
              ),
          human_proxies__shared_climate_selection__time_control =
            data_h1_proxy_record_climate |>
              dplyr::select(-dplyr::all_of("selection_audit")),
          human_proxies__shared_climate_selection__space_control =
            data_h1_proxy_slice_climate |>
              dplyr::select(-dplyr::all_of("selection_audit")),
          human_proxies__climate_retention__time_control =
            table_h1_proxy_climate_retention_records,
          human_proxies__climate_retention__space_control =
            table_h1_proxy_climate_retention_slices,
          human_proxies__inputs__provenance =
            table_h1_proxy_input_provenance
        )

      vec_paths <-
        file.path(path_tables, stringr::str_c(names(list_tables), ".csv")) |>
        rlang::set_names(names(list_tables))

      save_collinearity_managed_hvarpart_tables(list_tables, vec_paths)
    },
    format = "file"
  )
)
