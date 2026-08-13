#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#          Export reviewer spatial-dependence results
#
#                   O. Mottl, V.A. Felde
#                         2026
#
#----------------------------------------------------------#

library(here)

source(
  here::here(
    "R/00_Config_file.R"
  )
)

path_store <-
  file.path(
    data_storage_path,
    "Targets_data",
    "analyses_h1_reviewer_spatiotemporal_control"
  )
path_tables <-
  here::here(
    "Outputs",
    "Tables",
    "Spatial_dependence"
  )
path_spatial_figures <-
  here::here(
    "Outputs",
    "Figures",
    "H1",
    "Spatial"
  )

dir.create(path_tables, recursive = TRUE, showWarnings = FALSE)

dir.create(
  path_spatial_figures,
  recursive = TRUE,
  showWarnings = FALSE
)

list_tables <-
  list(
    dataset_age_collapse_filtered = targets::tar_read(
      table_dataset_age_collapse_filtered,
      store = path_store
    ),
    dataset_age_collapse_temporal = targets::tar_read(
      table_dataset_age_collapse_temporal,
      store = path_store
    ),
    time_control_status = targets::tar_read(
      table_time_control_status,
      store = path_store
    ),
    time_control_hierarchical_contributions = targets::tar_read(
      table_time_control_hierarchical_contributions,
      store = path_store
    ),
    time_control_unique_adjusted_r2 = targets::tar_read(
      table_time_control_unique_adjusted_r2,
      store = path_store
    ),
    time_control_residual_moran = targets::tar_read(
      table_time_control_residual_moran,
      store = path_store
    ),
    time_control_records_all = targets::tar_read(
      data_time_controlled_balance_records_all,
      store = path_store
    ),
    time_control_records = targets::tar_read(
      data_time_controlled_balance_records,
      store = path_store
    ),
    human_climate_only_matched_estimates = targets::tar_read(
      table_human_climate_only_matched_estimates,
      store = path_store
    ),
    spatiotemporal_balance_thinning = targets::tar_read(
      data_spatiotemporal_balance_thinning,
      store = path_store
    ),
    spatiotemporal_balance_sensitivity = targets::tar_read(
      table_spatiotemporal_balance_sensitivity,
      store = path_store
    ),
    spatiotemporal_balance_estimates = targets::tar_read(
      table_spatiotemporal_balance_estimates,
      store = path_store
    ),
    spatiotemporal_balance_moran = targets::tar_read(
      table_spatiotemporal_balance_moran,
      store = path_store
    ),
    spatiotemporal_balance_dbmem_diagnostics = targets::tar_read(
      table_spatiotemporal_balance_dbmem_diagnostics,
      store = path_store
    ),
    spatiotemporal_balance_dbmem_selection = targets::tar_read(
      table_spatiotemporal_balance_dbmem_selection,
      store = path_store
    ),
    spatiotemporal_balance_robustness = targets::tar_read(
      table_spatiotemporal_balance_robustness,
      store = path_store
    ),
    spatial_control_status = targets::tar_read(
      table_spatial_control_status,
      store = path_store
    ),
    spatial_control_dbmem_selection = targets::tar_read(
      table_spatial_control_dbmem_selection,
      store = path_store
    ),
    spatial_control_dbmem_diagnostics = targets::tar_read(
      table_spatial_control_dbmem_diagnostics,
      store = path_store
    ),
    spatial_control_hierarchical_contributions = targets::tar_read(
      table_spatial_control_hierarchical_contributions,
      store = path_store
    ),
    spatial_control_unique_adjusted_r2 = targets::tar_read(
      table_spatial_control_unique_adjusted_r2,
      store = path_store
    ),
    spatial_control_residual_moran = targets::tar_read(
      table_spatial_control_residual_moran,
      store = path_store
    ),
    spatial_control_remaining_spatial_test = targets::tar_read(
      table_spatial_control_remaining_signal,
      store = path_store
    ),
    spatial_control_zero_truncated_composition = targets::tar_read(
      table_spatial_control_zero_truncated_composition,
      store = path_store
    ),
    spatial_control_rankings = targets::tar_read(
      table_spatial_control_rankings,
      store = path_store
    ),
    spatial_importance_records = targets::tar_read(
      data_spatial_importance_records,
      store = path_store
    ),
    spatial_importance_thinning_ledger = targets::tar_read(
      data_spatial_importance_thinning,
      store = path_store
    ),
    spatial_importance_sensitivity = targets::tar_read(
      table_spatial_importance_sensitivity,
      store = path_store
    ),
    spatial_importance_estimates = targets::tar_read(
      table_spatial_importance_estimates,
      store = path_store
    ),
    spatial_importance_moran_diagnostics = targets::tar_read(
      table_spatial_importance_moran,
      store = path_store
    ),
    spatial_importance_dbmem_diagnostics = targets::tar_read(
      table_spatial_importance_dbmem_diagnostics,
      store = path_store
    ),
    spatial_importance_dbmem_selection = targets::tar_read(
      table_spatial_importance_dbmem_selection,
      store = path_store
    ),
    spatial_importance_robustness = targets::tar_read(
      table_spatial_importance_robustness,
      store = path_store
    ),
    temporal_spatial_status = targets::tar_read(
      table_temporal_spatial_status,
      store = path_store
    ),
    temporal_hierarchical_contributions = targets::tar_read(
      table_temporal_hvarpart_components,
      store = path_store
    ),
    temporal_hvarpart_rankings = targets::tar_read(
      table_temporal_hvarpart_rankings,
      store = path_store
    ),
    temporal_unique_adjusted_r2 = targets::tar_read(
      table_temporal_unique_adjusted_r2,
      store = path_store
    ),
    temporal_residual_moran = targets::tar_read(
      table_temporal_residual_moran,
      store = path_store
    ),
    temporal_remaining_spatial_test = targets::tar_read(
      table_temporal_remaining_spatial_test,
      store = path_store
    ),
    spatial_sensitivity_provenance = targets::tar_read(
      table_spatial_sensitivity_provenance,
      store = path_store
    ) |>
      dplyr::mutate(
        exported_at_utc = format(
          Sys.time(),
          tz = "UTC",
          usetz = TRUE
        )
      )
  )

purrr::iwalk(
  .x = list_tables,
  .f = ~ readr::write_csv(
    x = .x,
    file = file.path(
      path_tables,
      stringr::str_c(
        .y,
        dplyr::if_else(
          .y %in% c(
            "spatial_importance_thinning_ledger",
            "spatiotemporal_balance_thinning"
          ),
          ".csv.gz",
          ".csv"
        )
      )
    )
  )
)

list_plots <-
  plot_spatial_sensitivity_results(
    data_moran = list_tables[["spatial_importance_moran_diagnostics"]],
    data_sensitivity = list_tables[["spatial_importance_sensitivity"]],
    data_estimates = list_tables[["spatial_importance_estimates"]],
    data_temporal_components =
      list_tables[["temporal_hierarchical_contributions"]],
    data_unique_adjusted_r2 = list_tables[["temporal_unique_adjusted_r2"]]
  )

purrr::iwalk(
  .x = list_plots,
  .f = ~ {
    plot_object <- .x

    plot_name <- .y

    purrr::walk(
      .x = c("png", "pdf"),
      .f = ~ ggplot2::ggsave(
        filename = file.path(
          path_spatial_figures,
          stringr::str_c(plot_name, ".", .x)
        ),
        plot = plot_object,
        width = image_width_vec[["2col"]],
        height = 120,
        units = image_units,
        bg = "white"
      )
    )
  }
)

# end of script ----
