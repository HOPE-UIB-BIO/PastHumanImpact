#----------------------------------------------------------#
#
#                     GlobalHumanImpact
#
#      Single-proxy spatial H1 human-climate balance maps
#
#                   O. Mottl, V.A. Felde
#                         2026
#
#----------------------------------------------------------#
# Draws the canonical spatial-balance layout for each main human proxy in
#   the two decision-record cells: the main window (common cohort, records
#   shared by the three main proxies, SPD buffer) and the extension (native
#   window, all records of the run, SPD buffer). Reads the run stores
#   analyses_h1/<profile_id> and the store analyses_h1/human_proxy_outcomes.

#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

library(here)

source(here::here("R/00_Config_file.R"))

runner_h1_proxies <-
  "R/analyses/02_h1_spatiotemporal_hvarpart/human_proxies/00_run.R"

store_outcomes <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "analyses_h1/human_proxy_outcomes"
  )

path_figures <-
  here::here("Outputs", "Figures", "H1", "Proxies", "Spatial")

vec_proxy_labels <-
  c(
    spd = "SPD",
    kk10 = "KK10 land use",
    hyde_3_5_land_use = "HYDE 3.5 land use"
  )

data_figure_specs <-
  tidyr::expand_grid(
    human_proxy = names(vec_proxy_labels),
    age_window = c("common_cohort", "native")
  ) |>
  dplyr::mutate(
    record_set = dplyr::if_else(
      .data[["age_window"]] == "common_cohort",
      "shared_records",
      "all_records"
    ),
    profile_id = stringr::str_c(
      "within_dataset_", .data[["human_proxy"]], "_spd_buffer_",
      .data[["age_window"]], "_time_control"
    ),
    store = purrr::map_chr(
      .data[["profile_id"]],
      ~ resolve_pipeline_store_path(
        data_storage_path = data_storage_path,
        store_relative_path = file.path("analyses_h1", .x)
      )
    ),
    window_label = dplyr::if_else(
      .data[["age_window"]] == "common_cohort",
      "common cohort, 2.0–8.0 ka, records shared by the three proxies",
      "native window, all records of the run"
    ),
    fig_name = stringr::str_c(
      .data[["human_proxy"]],
      "human_climate_balance",
      "zero_truncated",
      .data[["age_window"]],
      .data[["record_set"]],
      sep = "__"
    )
  )

#----------------------------------------------------------#
# 1. Load inputs -----
#----------------------------------------------------------#

data_shared_records <-
  load_target_store_value(
    store = store_outcomes,
    target_name = "table_h1_proxy_comparison_units",
    runner = runner_h1_proxies
  ) |>
  dplyr::filter(
    .data[["analysis"]] == "spatial_h1",
    .data[["gridded_support"]] == "spd_buffer",
    .data[["age_window"]] == "common_cohort"
  ) |>
  dplyr::select(dplyr::all_of("dataset_id"))

data_comparison_estimates <-
  load_target_store_value(
    store = store_outcomes,
    target_name = "table_h1_proxy_comparison_balance_estimates",
    runner = runner_h1_proxies
  ) |>
  dplyr::filter(
    .data[["gridded_support"]] == "spd_buffer",
    .data[["age_window"]] == "common_cohort"
  )

data_geo_koppen <-
  readr::read_rds(
    file.path(
      data_storage_path,
      "Spatial",
      "Climatezones",
      "data_geo_koppen.rds"
    )
  ) |>
  tibble::as_tibble() |>
  dplyr::mutate(
    climatezone = dplyr::case_when(
      .data[["ecozone_koppen_15"]] == "Cold_Without_dry_season" ~
        .data[["ecozone_koppen_30"]],
      .data[["ecozone_koppen_5"]] %in% c("Cold", "Temperate") ~
        .data[["ecozone_koppen_15"]],
      .default = .data[["ecozone_koppen_5"]]
    )
  ) |>
  prepare_climatezone_factor()

#----------------------------------------------------------#
# 2. Build and save the maps -----
#----------------------------------------------------------#

dir.create(path_figures, recursive = TRUE, showWarnings = FALSE)

data_figure_specs |>
  dplyr::select(
    dplyr::all_of(c(
      "human_proxy", "age_window", "profile_id", "store", "window_label",
      "fig_name"
    ))
  ) |>
  purrr::pwalk(
    .f = ~ {
      human_proxy <- ..1

      age_window <- ..2

      profile_id <- ..3

      store <- ..4

      window_label <- ..5

      fig_name <- ..6

      data_records_run <-
        load_target_store_value(
          store = store,
          target_name = "data_h1_proxy_estimable_units",
          runner = runner_h1_proxies
        )

      # The main window uses the records shared by the three proxies and
      #    the estimates refitted on them; the extension uses the run's own.
      data_records <-
        if (
          identical(age_window, "common_cohort")
        ) {
          data_records_run |>
            dplyr::semi_join(data_shared_records, by = "dataset_id")
        } else {
          data_records_run
        }

      data_estimates <-
        if (
          identical(age_window, "common_cohort")
        ) {
          data_comparison_estimates |>
            dplyr::filter(.data[["profile_id"]] == .env[["profile_id"]])
        } else {
          load_target_store_value(
            store = store,
            target_name = "table_h1_proxy_balance_estimates",
            runner = runner_h1_proxies
          )
        }

      # A few records lie in a Koppen class outside the project's eleven
      #    regions; the map cannot place them, so they are left out of the
      #    figure (not of the estimates) and counted in the title.
      vec_is_mapped <-
        data_records |>
        dplyr::select(dplyr::all_of("climatezone")) |>
        prepare_climatezone_factor() |>
        dplyr::pull("climatezone") |>
        is.na() |>
        magrittr::not()

      data_records_mapped <-
        data_records[vec_is_mapped, ]

      vec_estimate_is_mapped <-
        data_estimates |>
        dplyr::select(dplyr::all_of("climatezone")) |>
        prepare_climatezone_factor() |>
        dplyr::pull("climatezone") |>
        is.na() |>
        magrittr::not() |>
        magrittr::or(dplyr::pull(data_estimates, "climatezone") == "All")

      data_estimates_mapped <-
        data_estimates[vec_estimate_is_mapped, ]

      n_unmapped <-
        nrow(data_records) - nrow(data_records_mapped)

      title_unmapped <-
        if (
          n_unmapped > 0L
        ) {
          stringr::str_c(
            "; ", n_unmapped, " in a region outside the 11 classes not mapped"
          )
        } else {
          ""
        }

      plot_balance <-
        plot_h1_spatial_controlled_balance(
          data_records = data_records_mapped,
          data_estimates = data_estimates_mapped,
          data_geo_koppen = data_geo_koppen,
          profile = "zero_truncated"
        )

      plot_titled <-
        cowplot::plot_grid(
          cowplot::ggdraw() +
            cowplot::draw_label(
              label = stringr::str_c(
                vec_proxy_labels[[human_proxy]], ": ", window_label, "\n",
                nrow(data_records), " records", title_unmapped
              ),
              size = text_size,
              x = 0.01,
              hjust = 0
            ),
          plot_balance,
          ncol = 1L,
          rel_heights = c(0.1, 1)
        )

      save_diagnostic_figure_formats(
        fig_object = plot_titled,
        fig_name = fig_name,
        path_figures = path_figures,
        width = image_width_vec[["2col"]],
        height = 140,
        units = image_units
      )
    }
  )
