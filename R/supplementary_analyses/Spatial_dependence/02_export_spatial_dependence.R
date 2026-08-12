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
    "analyses_h1_reviewer_spatial_dependence"
  )
path_tables <-
  here::here(
    "Outputs",
    "Tables",
    "Spatial_dependence"
  )
path_figures <-
  here::here(
    "Outputs",
    "Figures",
    "Extended_data_figures",
    "Spatial_dependence"
  )
dir.create(path_tables, recursive = TRUE, showWarnings = FALSE)
dir.create(path_figures, recursive = TRUE, showWarnings = FALSE)

list_tables <-
  list(
    figure2_spatial_records = targets::tar_read(
      data_figure2_spatial_records,
      store = path_store
    ),
    figure2_thinning_ledger = targets::tar_read(
      data_figure2_thinning_ledger,
      store = path_store
    ),
    figure2_spatial_sensitivity = targets::tar_read(
      table_figure2_spatial_sensitivity,
      store = path_store
    ),
    figure2_spatial_estimates = targets::tar_read(
      table_figure2_spatial_estimates,
      store = path_store
    ),
    figure2_moran_diagnostics = targets::tar_read(
      table_figure2_moran_diagnostics,
      store = path_store
    ),
    figure2_dbmem_diagnostics = targets::tar_read(
      table_figure2_dbmem_diagnostics,
      store = path_store
    ),
    figure2_dbmem_selection = targets::tar_read(
      table_figure2_dbmem_selection,
      store = path_store
    ),
    figure2_robustness = targets::tar_read(
      table_figure2_robustness,
      store = path_store
    ),
    temporal_spatial_status = targets::tar_read(
      table_temporal_spatial_status,
      store = path_store
    ),
    temporal_hvarpart_components = targets::tar_read(
      table_temporal_hvarpart_components,
      store = path_store
    ),
    temporal_hvarpart_rankings = targets::tar_read(
      table_temporal_hvarpart_rankings,
      store = path_store
    ),
    temporal_partial_fractions = targets::tar_read(
      table_temporal_partial_fractions,
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
        ifelse(.y == "figure2_thinning_ledger", ".csv.gz", ".csv")
      )
    )
  )
)

list_plots <-
  plot_spatial_sensitivity_results(
    data_moran = list_tables[["figure2_moran_diagnostics"]],
    data_sensitivity = list_tables[["figure2_spatial_sensitivity"]],
    data_estimates = list_tables[["figure2_spatial_estimates"]],
    data_temporal_components =
      list_tables[["temporal_hvarpart_components"]],
    data_partial = list_tables[["temporal_partial_fractions"]]
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
          path_figures,
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
