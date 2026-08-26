#----------------------------------------------------------#
#
#                     GlobalHumanImpact
#
#          Human-climate spatial balance analysis
#
#                   O. Mottl, V.A. Felde
#                         2026
#
#----------------------------------------------------------#

#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#
library(here)

source(here::here("R/00_Config_file.R"))

source(
  here::here(
    "R/analyses/01_data_preparation/01_metadata/02_metadata.R"
  )
)

#----------------------------------------------------------#
# 1. Load controlled results -----
#----------------------------------------------------------#
store_time_control_spd <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "analyses_h1/time_control/spd"
  )

store_time_control_events <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "analyses_h1/time_control/events"
  )

store_spatial_aggregation_spd <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = stringr::str_c(
      "analyses_h1/spatial_aggregation",
      "spd_human_climate_balance",
      sep = "/"
    )
  )

store_spatial_aggregation_events <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = stringr::str_c(
      "analyses_h1/spatial_aggregation",
      "events_human_climate_balance",
      sep = "/"
    )
  )

data_spd_records <-
  targets::tar_read(
    data_time_controlled_balance_records,
    store = store_time_control_spd
  )

data_spd_estimates <-
  targets::tar_read(
    table_spatiotemporal_balance_estimates,
    store = store_spatial_aggregation_spd
  )

data_spd_contributions <-
  targets::tar_read(
    table_time_control_hierarchical_contributions,
    store = store_time_control_spd
  )

data_spd_unique_adjusted_r2 <-
  targets::tar_read(
    table_time_control_unique_adjusted_r2,
    store = store_time_control_spd
  )

data_events_records <-
  targets::tar_read(
    data_time_controlled_balance_records,
    store = store_spatial_aggregation_events
  )

data_events_estimates <-
  targets::tar_read(
    table_spatiotemporal_balance_estimates,
    store = store_spatial_aggregation_events
  )

data_events_contributions <-
  targets::tar_read(
    table_time_control_hierarchical_contributions,
    store = store_time_control_events
  )

data_events_unique_adjusted_r2 <-
  targets::tar_read(
    table_time_control_unique_adjusted_r2,
    store = store_time_control_events
  )

data_geo_koppen <-
  readr::read_rds(
    stringr::str_c(
      data_storage_path,
      "Spatial/Climatezones/data_geo_koppen.rds"
    )
  ) |>
  tibble::as_tibble() |>
  dplyr::mutate(
    climatezone = dplyr::case_when(
      ecozone_koppen_15 == "Cold_Without_dry_season" ~ ecozone_koppen_30,
      ecozone_koppen_5 %in% c("Cold", "Temperate") ~ ecozone_koppen_15,
      .default = ecozone_koppen_5
    )
  ) |>
  prepare_climatezone_factor()

#----------------------------------------------------------#
# 2. Build balance and component figures -----
#----------------------------------------------------------#
plot_spd_zero_truncated_balance <-
  plot_h1_spatial_controlled_balance(
    data_records = data_spd_records,
    data_estimates = data_spd_estimates,
    data_geo_koppen = data_geo_koppen,
    profile = "zero_truncated"
  )

plot_spd_untruncated_balance <-
  plot_h1_spatial_controlled_balance(
    data_records = data_spd_records,
    data_estimates = data_spd_estimates,
    data_geo_koppen = data_geo_koppen,
    profile = "signed"
  )

plot_events_zero_truncated_balance <-
  plot_h1_spatial_controlled_balance(
    data_records = data_events_records,
    data_estimates = data_events_estimates,
    data_geo_koppen = data_geo_koppen,
    profile = "zero_truncated"
  )

plot_events_untruncated_balance <-
  plot_h1_spatial_controlled_balance(
    data_records = data_events_records,
    data_estimates = data_events_estimates,
    data_geo_koppen = data_geo_koppen,
    profile = "signed"
  )

plots_spd_control_profiles <-
  build_h1_spatial_control_profiles(
    data_records = data_spd_records,
    data_components = data_spd_contributions,
    data_unique_adjusted_r2 = data_spd_unique_adjusted_r2,
    data_geo_koppen = data_geo_koppen
  )

plots_spd_spatiotemporal_composition <-
  plot_h1_spatial_control_profiles(
    data_records = data_spd_records,
    data_components = data_spd_contributions,
    data_unique_adjusted_r2 = data_spd_unique_adjusted_r2
  )

composition_profile_names <-
  c(
    "zero_truncated_hierarchical_composition",
    "unique_adjusted_r2"
  )

plots_spd_spatiotemporal_composition <-
  plots_spd_spatiotemporal_composition[composition_profile_names] |>
  rlang::set_names(
    stringr::str_c(
      "spd",
      "human_climate_time",
      composition_profile_names,
      sep = "__"
    )
  )

plots_events_control_profiles <-
  build_h1_spatial_control_profiles(
    data_records = data_events_records,
    data_components = data_events_contributions,
    data_unique_adjusted_r2 = data_events_unique_adjusted_r2,
    data_geo_koppen = data_geo_koppen
  )

component_output_names <-
  c(
    climate_unique_adjusted_r2 =
      "climate__unique_adjusted_r2__time_control",
    human_unique_adjusted_r2 =
      "human__unique_adjusted_r2__time_control",
    time_unique_adjusted_r2 =
      "time__unique_adjusted_r2__time_control",
    climate_untruncated_hierarchical_contribution =
      stringr::str_c(
        "climate",
        "untruncated_hierarchical_contribution",
        "time_control",
        sep = "__"
      ),
    human_untruncated_hierarchical_contribution =
      stringr::str_c(
        "human",
        "untruncated_hierarchical_contribution",
        "time_control",
        sep = "__"
      ),
    time_untruncated_hierarchical_contribution =
      stringr::str_c(
        "time",
        "untruncated_hierarchical_contribution",
        "time_control",
        sep = "__"
      )
  )

balance_output_names <-
  c(
    stringr::str_c(
      "human_climate_balance",
      "zero_truncated_hierarchical_composition",
      "time_and_space_control",
      sep = "__"
    ),
    stringr::str_c(
      "human_climate_balance",
      "untruncated_hierarchical_contribution_difference",
      "time_and_space_control",
      sep = "__"
    )
  )

plots_spd_components <-
  plots_spd_control_profiles |>
  purrr::map(~ .x[["plot"]]) |>
  rlang::set_names(
    stringr::str_c(
      "spd",
      unname(component_output_names[names(plots_spd_control_profiles)]),
      sep = "__"
    )
  )

plots_events_components <-
  plots_events_control_profiles |>
  purrr::map(~ .x[["plot"]]) |>
  rlang::set_names(
    stringr::str_c(
      "events",
      unname(component_output_names[names(plots_events_control_profiles)]),
      sep = "__"
    )
  )

plots_spd <-
  c(
    list(
      plot_spd_zero_truncated_balance,
      plot_spd_untruncated_balance
    ) |>
      rlang::set_names(
        stringr::str_c("spd", balance_output_names, sep = "__")
      ),
    plots_spd_components
  )

plots_events <-
  c(
    list(
      plot_events_zero_truncated_balance,
      plot_events_untruncated_balance
    ) |>
      rlang::set_names(
        stringr::str_c("events", balance_output_names, sep = "__")
      ),
    plots_events_components
  )
#----------------------------------------------------------#
# 3. Prepare source tables -----
#----------------------------------------------------------#
data_spd_profile_records <-
  plots_spd_control_profiles |>
  purrr::imap(
    ~ .x[["record_values"]] |>
      dplyr::mutate(figure_profile = .y)
  ) |>
  dplyr::bind_rows()

data_spd_profile_climatezones <-
  plots_spd_control_profiles |>
  purrr::imap(
    ~ .x[["climatezone_values"]] |>
      dplyr::mutate(figure_profile = .y)
  ) |>
  dplyr::bind_rows()

data_spd_profile_regions <-
  plots_spd_control_profiles |>
  purrr::imap(
    ~ .x[["region_values"]] |>
      dplyr::mutate(figure_profile = .y)
  ) |>
  dplyr::bind_rows()

data_events_profile_records <-
  plots_events_control_profiles |>
  purrr::imap(
    ~ .x[["record_values"]] |>
      dplyr::mutate(figure_profile = .y)
  ) |>
  dplyr::bind_rows()

data_events_profile_climatezones <-
  plots_events_control_profiles |>
  purrr::imap(
    ~ .x[["climatezone_values"]] |>
      dplyr::mutate(figure_profile = .y)
  ) |>
  dplyr::bind_rows()

data_events_profile_regions <-
  plots_events_control_profiles |>
  purrr::imap(
    ~ .x[["region_values"]] |>
      dplyr::mutate(figure_profile = .y)
  ) |>
  dplyr::bind_rows()

#----------------------------------------------------------#
# 4. Save figures and source tables -----
#----------------------------------------------------------#
path_data_spd <-
  here::here("Outputs", "Tables", "H1", "Spatial", "SPD")

path_data_events <-
  here::here("Outputs", "Tables", "H1", "Spatial", "Events")

dir.create(
  path_data_spd,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  path_data_events,
  recursive = TRUE,
  showWarnings = FALSE
)

path_figures_spd <-
  here::here("Outputs/Figures/H1/Spatial/SPD")

path_figures_events <-
  here::here("Outputs/Figures/H1/Spatial/Events")

path_figures_spd_composition <-
  file.path(path_figures_spd, "Spatiotemporal_composition")

dir.create(
  path_figures_spd,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  path_figures_events,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  path_figures_spd_composition,
  recursive = TRUE,
  showWarnings = FALSE
)

plots_by_proxy <-
  list(
    SPD = plots_spd,
    Events = plots_events
  )

paths_by_proxy <-
  c(
    SPD = path_figures_spd,
    Events = path_figures_events
  )

purrr::iwalk(
  .x = plots_by_proxy,
  .f = ~ {
    proxy_name <- .y

    purrr::walk(
      .x = c("png", "pdf"),
      .f = ~ {
        extension <- .x

        purrr::iwalk(
          .x = plots_by_proxy[[proxy_name]],
          .f = ~ ggplot2::ggsave(
            filename = file.path(
              paths_by_proxy[[proxy_name]],
              stringr::str_c(.y, ".", extension)
            ),
            plot = .x,
            width = image_width_vec[["2col"]],
            height = 130,
            units = image_units,
            bg = "white"
          )
        )
      }
    )
  }
)

purrr::walk(
  .x = c("png", "pdf"),
  .f = ~ {
    extension <- .x

    purrr::iwalk(
      .x = plots_spd_spatiotemporal_composition,
      .f = ~ ggplot2::ggsave(
        filename = file.path(
          path_figures_spd_composition,
          stringr::str_c(.y, ".", extension)
        ),
        plot = .x,
        width = image_width_vec[["2col"]],
        height = 190,
        units = image_units,
        bg = "white"
      )
    )
  }
)

readr::write_csv(
  data_spd_profile_records,
  file.path(
    path_data_spd,
    stringr::str_c(
      "spd__human_climate_time__component_profiles__",
      "dataset_values__time_control.csv"
    )
  )
)

readr::write_csv(
  data_spd_profile_climatezones,
  file.path(
    path_data_spd,
    stringr::str_c(
      "spd__human_climate_time__component_profiles__",
      "climate_zone_values__time_control.csv"
    )
  )
)

readr::write_csv(
  data_spd_profile_regions,
  file.path(
    path_data_spd,
    stringr::str_c(
      "spd__human_climate_time__component_profiles__",
      "region_values__time_control.csv"
    )
  )
)

readr::write_csv(
  data_spd_records,
  file.path(
    path_data_spd,
    stringr::str_c(
      "spd__human_climate_balance__dataset_values__",
      "time_and_space_control.csv"
    )
  )
)

readr::write_csv(
  data_spd_estimates,
  file.path(
    path_data_spd,
    stringr::str_c(
      "spd__human_climate_balance__climate_zone_values__",
      "time_and_space_control.csv"
    )
  )
)

readr::write_csv(
  data_events_profile_records,
  file.path(
    path_data_events,
    stringr::str_c(
      "events__human_climate_time__component_profiles__",
      "dataset_values__time_control.csv"
    )
  )
)

readr::write_csv(
  data_events_profile_climatezones,
  file.path(
    path_data_events,
    stringr::str_c(
      "events__human_climate_time__component_profiles__",
      "climate_zone_values__time_control.csv"
    )
  )
)

readr::write_csv(
  data_events_profile_regions,
  file.path(
    path_data_events,
    stringr::str_c(
      "events__human_climate_time__component_profiles__",
      "region_values__time_control.csv"
    )
  )
)

readr::write_csv(
  data_events_records,
  file.path(
    path_data_events,
    stringr::str_c(
      "events__human_climate_balance__dataset_values__",
      "time_and_space_control.csv"
    )
  )
)

readr::write_csv(
  data_events_estimates,
  file.path(
    path_data_events,
    stringr::str_c(
      "events__human_climate_balance__climate_zone_values__",
      "time_and_space_control.csv"
    )
  )
)
