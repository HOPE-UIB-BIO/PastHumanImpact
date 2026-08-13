#----------------------------------------------------------#
#
#                     GlobalHumanImpact
#
#         Export HVarPart relative importance
#
#                         2026
#
#----------------------------------------------------------#

#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

library(here)
source(
  here::here("R/00_Config_file.R")
)
source(
  here::here("R/main_analysis/02_meta_data.R")
)

#----------------------------------------------------------#
# 1. Load H1 model outputs -----
#----------------------------------------------------------#

path_store_h1 <-
  stringr::str_c(
    data_storage_path,
    "Targets_data/analyses_h1"
  )

data_h1_importance <-
  dplyr::bind_rows(
    compute_hvarpart_importance(
      targets::tar_read(
        "output_spatial_spd",
        store = path_store_h1
      ) |>
        dplyr::left_join(
          data_meta |>
            dplyr::select(
              .data[["dataset_id"]],
              .data[["region"]],
              .data[["climatezone"]]
            ),
          by = "dataset_id"
        ) |>
        dplyr::mutate(analysis = "spatial_spd"),
      id_cols = c(
        "analysis",
        "dataset_id",
        "region",
        "climatezone"
      )
    ),
    compute_hvarpart_importance(
      targets::tar_read(
        "output_spatial_events",
        store = path_store_h1
      ) |>
        dplyr::left_join(
          data_meta |>
            dplyr::select(
              .data[["dataset_id"]],
              .data[["region"]],
              .data[["climatezone"]]
            ),
          by = "dataset_id"
        ) |>
        dplyr::mutate(analysis = "spatial_events"),
      id_cols = c(
        "analysis",
        "dataset_id",
        "region",
        "climatezone"
      )
    ),
    compute_hvarpart_importance(
      targets::tar_read(
        "output_temporal_spd",
        store = path_store_h1
      ) |>
        dplyr::filter(
          dplyr::between(
            .data[["age"]],
            2000,
            8500
          )
        ) |>
        dplyr::mutate(analysis = "temporal_spd"),
      id_cols = c(
        "analysis",
        "region",
        "age"
      )
    ),
    compute_hvarpart_importance(
      targets::tar_read(
        "output_temporal_events",
        store = path_store_h1
      ) |>
        dplyr::mutate(analysis = "temporal_events"),
      id_cols = c(
        "analysis",
        "region",
        "age"
      )
    )
  )

#----------------------------------------------------------#
# 2. Load H2 model outputs -----
#----------------------------------------------------------#

path_store_h2 <-
  stringr::str_c(
    data_storage_path,
    "Targets_data/analyses_h2"
  )

data_h2_importance <-
  compute_hvarpart_importance(
    targets::tar_read(
      "output_hvar_h2_spd",
      store = path_store_h2
    ) |>
      dplyr::mutate(analysis = "h2_spd"),
    id_cols = c(
      "analysis",
      "region",
      "climatezone"
    )
  )

#----------------------------------------------------------#
# 3. Combine importance data -----
#----------------------------------------------------------#

vec_analysis_export_order <-
  c(
    "temporal_spd",
    "temporal_events",
    "spatial_spd",
    "spatial_events",
    "h2_spd"
  )

data_importance <-
  dplyr::bind_rows(
    data_h1_importance,
    data_h2_importance
  ) |>
  dplyr::arrange(
    match(
      .data[["analysis"]],
      vec_analysis_export_order
    ),
    .data[["model_id"]],
    .data[["predictor"]]
  )

#----------------------------------------------------------#
# 4. Summarise audits and profiles -----
#----------------------------------------------------------#

table_audit <-
  dplyr::bind_rows(
    summarise_hvarpart_audit(
      data_importance = data_importance,
      group_vars = "analysis"
    ) |>
      dplyr::mutate(aggregation_level = "analysis"),
    summarise_hvarpart_audit(
      data_importance = data_h1_importance |>
        dplyr::filter(
          stringr::str_starts(
            .data[["analysis"]],
            "spatial"
          )
        ),
      group_vars = c(
        "analysis",
        "region",
        "climatezone"
      )
    ) |>
      dplyr::mutate(aggregation_level = "region_climatezone"),
    summarise_hvarpart_audit(
      data_importance = data_h1_importance |>
        dplyr::filter(
          stringr::str_starts(
            .data[["analysis"]],
            "temporal"
          )
        ),
      group_vars = c(
        "analysis",
        "region",
        "age"
      )
    ) |>
      dplyr::mutate(aggregation_level = "region_age"),
    summarise_hvarpart_audit(
      data_importance = data_h2_importance,
      group_vars = c(
        "analysis",
        "region",
        "climatezone"
      )
    ) |>
      dplyr::mutate(aggregation_level = "region_climatezone")
  ) |>
  dplyr::relocate(
    dplyr::all_of("aggregation_level"),
    .after = dplyr::all_of("analysis")
  )

table_profiles <-
  dplyr::bind_rows(
    diagnose_hvarpart_importance_profiles(
      data_importance = data_importance,
      group_vars = "analysis"
    ) |>
      dplyr::mutate(aggregation_level = "analysis"),
    diagnose_hvarpart_importance_profiles(
      data_importance = data_importance,
      group_vars = c(
        "analysis",
        "model_id"
      )
    ) |>
      dplyr::mutate(aggregation_level = "model"),
    diagnose_hvarpart_importance_profiles(
      data_importance = data_h1_importance |>
        dplyr::filter(
          stringr::str_starts(
            .data[["analysis"]],
            "spatial"
          )
        ),
      group_vars = c(
        "analysis",
        "region",
        "climatezone"
      )
    ) |>
      dplyr::mutate(aggregation_level = "region_climatezone"),
    diagnose_hvarpart_importance_profiles(
      data_importance = data_h1_importance |>
        dplyr::filter(
          stringr::str_starts(
            .data[["analysis"]],
            "temporal"
          )
        ),
      group_vars = c(
        "analysis",
        "region",
        "age"
      )
    ) |>
      dplyr::mutate(aggregation_level = "region_age"),
    diagnose_hvarpart_importance_profiles(
      data_importance = data_h2_importance,
      group_vars = c(
        "analysis",
        "region",
        "climatezone"
      )
    ) |>
      dplyr::mutate(aggregation_level = "region_climatezone")
  ) |>
  dplyr::relocate(
    dplyr::all_of("aggregation_level"),
    .after = dplyr::all_of("analysis")
  )

#----------------------------------------------------------#
# 5. Prepare output directories -----
#----------------------------------------------------------#

path_output_tables <-
  here::here("Outputs/Tables/HVarPart")

path_output_figures <-
  here::here(
    "Outputs/Figures/Diagnostics/HVarPart"
  )

dir.create(
  path = path_output_tables,
  recursive = TRUE,
  showWarnings = FALSE
)
dir.create(
  path = path_output_figures,
  recursive = TRUE,
  showWarnings = FALSE
)

#----------------------------------------------------------#
# 6. Export source tables -----
#----------------------------------------------------------#

readr::write_csv(
  x = data_importance,
  file = file.path(
    path_output_tables,
    "hvarpart_components.csv"
  )
)
readr::write_csv(
  x = table_audit,
  file = file.path(
    path_output_tables,
    "hvarpart_model_audit.csv"
  )
)
readr::write_csv(
  x = table_profiles,
  file = file.path(
    path_output_tables,
    "hvarpart_profile_comparison.csv"
  )
)

#----------------------------------------------------------#
# 7. Export profile-comparison figure -----
#----------------------------------------------------------#

plot_profiles <-
  plot_hvarpart_profile_comparison(table_profiles)

vec_figure_extensions <-
  c(
    "png",
    "pdf"
  )

purrr::walk(
  .x = vec_figure_extensions,
  .f = function(extension) {
    path_figure <-
      file.path(
        path_output_figures,
        stringr::str_c(
          "hvarpart_profile_comparison.",
          extension
        )
      )

    ggplot2::ggsave(
      filename = path_figure,
      plot = plot_profiles,
      width = image_width_vec[["2col"]],
      height = 110,
      units = image_units,
      bg = "white"
    )
  }
)
