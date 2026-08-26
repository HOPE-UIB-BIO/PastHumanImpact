#----------------------------------------------------------#
# Export H1 spatiotemporal-dependence diagnostics
#----------------------------------------------------------#

library(here)

source(here::here("R/00_Config_file.R"))

path_tables <-
  here::here("Outputs", "Tables", "Diagnostics", "Spatiotemporal_dependence")

dir.create(path_tables, recursive = TRUE, showWarnings = FALSE)

store_inputs <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "analyses_h1/inputs"
  )

store_time_spd <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "analyses_h1/time_control/spd"
  )

store_time_events <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "analyses_h1/time_control/events"
  )

store_space_spd <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "analyses_h1/spatial_control/spd"
  )

store_space_events <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "analyses_h1/spatial_control/events"
  )

store_aggregation <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = paste(
      "analyses_h1/spatial_aggregation",
      "spd_human_climate_balance",
      sep = "/"
    )
  )

store_robustness <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = paste(
      "sensitivity_analyses",
      "spatiotemporal_robustness",
      sep = "/"
    )
  )

list_tables <-
  list(
    dataset_age_collapse_filtered = targets::tar_read_raw(
      "table_dataset_age_collapse_filtered",
      store = store_inputs
    ),
    dataset_age_collapse_temporal = targets::tar_read_raw(
      "table_dataset_age_collapse_temporal",
      store = store_inputs
    ),
    time_control_status = dplyr::bind_rows(
      targets::tar_read_raw(
        "table_time_control_status",
        store = store_time_spd
      ),
      targets::tar_read_raw(
        "table_time_control_status",
        store = store_time_events
      )
    ),
    time_control_hierarchical_contributions = dplyr::bind_rows(
      targets::tar_read_raw(
        "table_time_control_hierarchical_contributions",
        store = store_time_spd
      ),
      targets::tar_read_raw(
        "table_time_control_hierarchical_contributions",
        store = store_time_events
      )
    ),
    time_control_unique_adjusted_r2 = dplyr::bind_rows(
      targets::tar_read_raw(
        "table_time_control_unique_adjusted_r2",
        store = store_time_spd
      ),
      targets::tar_read_raw(
        "table_time_control_unique_adjusted_r2",
        store = store_time_events
      )
    ),
    time_control_residual_moran = dplyr::bind_rows(
      targets::tar_read_raw(
        "table_time_control_residual_moran",
        store = store_time_spd
      ),
      targets::tar_read_raw(
        "table_time_control_residual_moran",
        store = store_time_events
      )
    ),
    spatiotemporal_balance_estimates = targets::tar_read_raw(
      "table_spatiotemporal_balance_estimates",
      store = store_aggregation
    ),
    spatiotemporal_balance_moran = targets::tar_read_raw(
      "table_spatiotemporal_balance_moran",
      store = store_aggregation
    ),
    spatial_control_status = dplyr::bind_rows(
      targets::tar_read_raw(
        "table_spatial_control_status",
        store = store_space_spd
      ),
      targets::tar_read_raw(
        "table_spatial_control_status",
        store = store_space_events
      )
    ),
    spatial_control_dbmem_selection = dplyr::bind_rows(
      targets::tar_read_raw(
        "table_spatial_control_dbmem_selection",
        store = store_space_spd
      ),
      targets::tar_read_raw(
        "table_spatial_control_dbmem_selection",
        store = store_space_events
      )
    ),
    spatial_control_residual_moran = dplyr::bind_rows(
      targets::tar_read_raw(
        "table_spatial_control_residual_moran",
        store = store_space_spd
      ),
      targets::tar_read_raw(
        "table_spatial_control_residual_moran",
        store = store_space_events
      )
    ),
    spatiotemporal_balance_sensitivity = targets::tar_read_raw(
      "table_spatiotemporal_balance_sensitivity",
      store = store_robustness
    ),
    spatiotemporal_balance_robustness = targets::tar_read_raw(
      "table_spatiotemporal_balance_robustness",
      store = store_robustness
    )
  )

vec_output_names <-
  c(
    dataset_age_collapse_filtered =
      "dataset_age__collapse_summary__filtered",
    dataset_age_collapse_temporal =
      "dataset_age__collapse_summary__temporal",
    time_control_status =
      "spd_events__human_climate_time__status__time_control",
    time_control_hierarchical_contributions = stringr::str_c(
      "spd_events__human_climate_time__",
      "hierarchical_contributions__time_control"
    ),
    time_control_unique_adjusted_r2 = stringr::str_c(
      "spd_events__human_climate_time__",
      "unique_adjusted_r2__time_control"
    ),
    time_control_residual_moran = stringr::str_c(
      "spd_events__human_climate_time__",
      "residual_moran__time_control"
    ),
    spatiotemporal_balance_estimates = stringr::str_c(
      "spd__human_climate_balance__estimates__",
      "time_and_space_control"
    ),
    spatiotemporal_balance_moran = stringr::str_c(
      "spd__human_climate_balance__moran_diagnostics__",
      "time_and_space_control"
    ),
    spatial_control_status =
      "spd_events__human_climate_space__status__space_control",
    spatial_control_dbmem_selection = stringr::str_c(
      "spd_events__human_climate_space__",
      "dbmem_selection__space_control"
    ),
    spatial_control_residual_moran = stringr::str_c(
      "spd_events__human_climate_space__",
      "residual_moran__space_control"
    ),
    spatiotemporal_balance_sensitivity = stringr::str_c(
      "spd__human_climate_balance__sensitivity__",
      "time_and_space_control"
    ),
    spatiotemporal_balance_robustness = stringr::str_c(
      "spd__human_climate_balance__robustness__",
      "time_and_space_control"
    )
  )

purrr::iwalk(
  .x = list_tables,
  .f = ~ readr::write_csv(
    x = .x,
    file = file.path(
      path_tables,
      stringr::str_c(vec_output_names[[.y]], ".csv")
    )
  )
)
