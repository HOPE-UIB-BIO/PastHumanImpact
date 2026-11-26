#----------------------------------------------------------#
#
#                     GlobalHumanImpact
#
#     Single-proxy temporal H1 human-climate-space composition
#
#                   O. Mottl, V.A. Felde
#                         2026
#
#----------------------------------------------------------#
# Draws the zero-truncated human, climate, and space composition of every
#   continental-region-by-age unit for each main human proxy in the two
#   decision-record cells: the main window (common cohort, units shared by
#   the three main proxies, SPD buffer) and the extension (native window,
#   all units of the run, SPD buffer). Reads the inputs store, the run stores
#   analyses_h1/<profile_id>, and the store analyses_h1/human_proxy_outcomes.

#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

library(here)

source(here::here("R/00_Config_file.R"))

runner_h1_proxies <-
  "R/analyses/02_h1_spatiotemporal_hvarpart/human_proxies/00_run.R"

store_inputs <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "analyses_h1/human_proxy_inputs"
  )

store_outcomes <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "analyses_h1/human_proxy_outcomes"
  )

path_figures <-
  here::here("Outputs", "Figures", "H1", "Proxies", "Temporal")

vec_proxy_labels <-
  c(
    spd = "SPD",
    kk10 = "KK10 land use",
    hyde_3_5_land_use = "HYDE 3.5 land use"
  )

#----------------------------------------------------------#
# 1. Load inputs -----
#----------------------------------------------------------#

data_windows <-
  load_target_store_value(
    store = store_inputs,
    target_name = "data_h1_proxy_windows",
    runner = runner_h1_proxies
  )

data_shared_units <-
  load_target_store_value(
    store = store_outcomes,
    target_name = "table_h1_proxy_comparison_units",
    runner = runner_h1_proxies
  ) |>
  dplyr::filter(
    .data[["analysis"]] == "temporal_h1",
    .data[["gridded_support"]] == "spd_buffer",
    .data[["age_window"]] == "common_cohort"
  ) |>
  dplyr::select(dplyr::all_of(c("region", "age")))

data_figure_specs <-
  tidyr::expand_grid(
    human_proxy = names(vec_proxy_labels),
    age_window = c("common_cohort", "native")
  ) |>
  dplyr::left_join(
    data_windows |>
      dplyr::select(
        dplyr::all_of(c(
          "human_proxy", "age_window", "age_min_bp", "age_max_bp"
        ))
      ),
    by = c("human_proxy", "age_window")
  ) |>
  dplyr::mutate(
    record_set = dplyr::if_else(
      .data[["age_window"]] == "common_cohort",
      "shared_records",
      "all_records"
    ),
    profile_id = stringr::str_c(
      "time_slice_", .data[["human_proxy"]], "_spd_buffer_",
      .data[["age_window"]], "_spatial_control"
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
      "common cohort, units shared by the three proxies",
      "native window, all units of the run"
    ),
    fig_width = dplyr::if_else(
      .data[["age_window"]] == "common_cohort",
      image_width_vec[["2col"]],
      image_width_vec[["3col"]]
    ),
    fig_name = stringr::str_c(
      .data[["human_proxy"]],
      "human_climate_space",
      "zero_truncated_hierarchical_composition",
      .data[["age_window"]],
      .data[["record_set"]],
      sep = "__"
    )
  )

#----------------------------------------------------------#
# 2. Build and save the compositions -----
#----------------------------------------------------------#

dir.create(path_figures, recursive = TRUE, showWarnings = FALSE)

data_figure_specs |>
  dplyr::select(
    dplyr::all_of(c(
      "human_proxy", "age_window", "age_min_bp", "age_max_bp", "store",
      "window_label", "fig_width", "fig_name"
    ))
  ) |>
  purrr::pwalk(
    .f = ~ {
      human_proxy <- ..1

      age_window <- ..2

      age_min_bp <- ..3

      age_max_bp <- ..4

      store <- ..5

      window_label <- ..6

      fig_width <- ..7

      fig_name <- ..8

      data_units_run <-
        load_target_store_value(
          store = store,
          target_name = "data_h1_proxy_estimable_units",
          runner = runner_h1_proxies
        )

      # The main window shows the units shared by the three proxies, the
      #    units of its decision metric; the extension shows the run's own.
      data_units <-
        if (
          identical(age_window, "common_cohort")
        ) {
          data_units_run |>
            dplyr::semi_join(data_shared_units, by = c("region", "age"))
        } else {
          data_units_run
        }

      n_units <-
        data_units |>
        dplyr::distinct(.data[["region"]], .data[["age"]]) |>
        nrow()

      plot_composition <-
        plot_h1_temporal_joint_human_proxy_composition(
          data_stack = data_units,
          age_min = age_min_bp,
          age_max = age_max_bp,
          human_label = stringr::str_c(
            "Human (", vec_proxy_labels[[human_proxy]], ")"
          )
        ) +
        # Shorter region labels fit the panel rows, and the axis title is
        #    moved below the age arrow drawn under the panels.
        ggplot2::theme(
          strip.text.y.left = ggplot2::element_text(
            size = text_size * 0.7,
            angle = 90
          ),
          axis.title.x = ggplot2::element_text(
            margin = ggplot2::margin(t = 7, unit = "mm")
          )
        )

      plot_titled <-
        cowplot::plot_grid(
          cowplot::ggdraw() +
            cowplot::draw_label(
              label = stringr::str_c(
                vec_proxy_labels[[human_proxy]], ": ", window_label, ", ",
                scales::number(age_min_bp / 1000, accuracy = 0.1), "–",
                scales::number(age_max_bp / 1000, accuracy = 0.1), " ka\n",
                n_units, " continental-region-by-age units"
              ),
              size = text_size,
              x = 0.01,
              hjust = 0
            ),
          plot_composition,
          ncol = 1L,
          rel_heights = c(0.12, 1)
        )

      save_diagnostic_figure_formats(
        fig_object = plot_titled,
        fig_name = fig_name,
        path_figures = path_figures,
        width = fig_width,
        height = 145,
        units = image_units
      )
    }
  )
