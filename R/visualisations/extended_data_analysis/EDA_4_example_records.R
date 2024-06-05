#----------------------------------------------------------#
#
#
#                   GlobalHumanImpact
#
#                    VISUALISATION
#           Supplementary: Example records
#
#                   V. Felde, O. Mottl
#                         2024
#
#----------------------------------------------------------#


#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

library(here)

# Load configuration
source(
  here::here(
    "R/00_Config_file.R"
  )
)

# Load meta data
source(
  here::here(
    "R/main_analysis/02_meta_data.R"
  )
)


#----------------------------------------------------------#
# 1. Load data -----
#----------------------------------------------------------#

data_h1_results <-
  targets::tar_read(
    name = "output_spatial_spd",
    store = paste0(
      data_storage_path,
      "Targets_data/analyses_h1"
    )
  )

data_pollen <-
  targets::tar_read(
    name = "data_pollen",
    store = paste0(
      data_storage_path,
      "Targets_data/pipeline_pollen_data"
    )
  )

#----------------------------------------------------------#
# 2. Data Wrangling -----
#----------------------------------------------------------#

data_meta_analysis <-
  data_h1_results %>%
  dplyr::select(dataset_id) %>%
  dplyr::inner_join(data_meta, by = "dataset_id")


data_pollen_analysis <-
  data_pollen %>%
  dplyr::filter(dataset_id %in% data_meta_analysis$dataset_id)

data_to_plot <-
  data_meta_analysis %>%
  dplyr::left_join(
    data_pollen_analysis,
    by = "dataset_id"
  ) %>%
  dplyr::left_join(
    data_h1_results,
    by = "dataset_id"
  ) %>%
  add_region_as_factor() %>%
  add_climatezone_as_factor() %>%
  dplyr::mutate(
    data_pollen = purrr::map2(
      .progress = TRUE,
      .x = levels,
      .y = counts_harmonised,
      .f = ~ dplyr::inner_join(
        .x %>%
          dplyr::select(
            sample_id,
            age
          ),
        .y,
        by = "sample_id"
      ) %>%
        dplyr::select(-sample_id)
    ),
    data_merge = purrr::map(
      .progress = TRUE,
      .x = data_merge,
      .f = ~ .x %>%
        dplyr::rename(
          "N0" = n0,
          "N1" = n1,
          "N2" = n2,
          "N1 minus N2" = n1_minus_n2,
          "N2 divided by N1" = n2_divided_by_n1,
          "N1 divided by N0" = n1_divided_by_n0,
          "DCCA axis 1" = dcca_axis_1,
          "ROC" = roc,
          "Density diversity" = density_diversity,
          "Density turnover" = density_turnover,
          "SPD" = spd,
          "MAT" = temp_annual,
          "MTCO" = temp_cold,
          "MAPS" = prec_summer,
          "MAPW" = prec_win
        )
    ),
    total_explained_variation = purrr::map_dbl(
      .progress = TRUE,
      .x = varhp,
      .f = purrr::possibly(
        ~ .x %>% purrr::chuck("varhp_output", "Total_explained_variation"),
        otherwise = NA_real_
      )
    ),
    varpar_summary_table = purrr::map(
      .progress = TRUE,
      .x = varhp,
      .f = purrr::possibly(
        ~ .x$summary_table %>%
          janitor::clean_names(),
        otherwise = tibble::tibble()
      )
    ),
  ) %>%
  dplyr::select(
    dataset_id,
    region,
    climatezone,
    data_pollen,
    data_merge,
    total_explained_variation,
    varpar_summary_table
  )


#----------------------------------------------------------#
# 4. Helper function -----
#----------------------------------------------------------#

plot_example_figure <- function(
    data_source,
    sel_example_record,
    time_step = 1) {
  p0 <-
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = age,
        y = value
      )
    ) +
    ggplot2::facet_wrap(
      ~predictor,
      nrow = 1,
      scales = "free_x",
      labeller = ggplot2::labeller(
        predictor = ggplot2::label_wrap_gen(5)
      )
    ) +
    ggplot2::scale_y_continuous(
      breaks = scales::pretty_breaks(n = 3)
    ) +
    ggplot2::scale_x_continuous(
      trans = "reverse",
      breaks = seq(2e3, 8.5e3, time_step * 1e3),
      labels = seq(2, 8.5, time_step)
    ) +
    ggplot2::coord_flip(
      xlim = c(8.5e3, 2e3)
    ) +
    ggplot2::labs(
      x = "Age (ka cal yr BP)",
      y = "Value"
    ) +
    ggplot2::theme(
      plot.margin = grid::unit(c(0, 1, 0, 1), "mm"),
      panel.spacing.y = grid::unit(3, "mm"),
      legend.position = "none",
      legend.text = ggplot2::element_text(
        size = text_size, # [config criteria]
        color = common_gray # [config criteria]
      ),
      legend.title = ggplot2::element_text(
        size = text_size, # [config criteria]
        color = common_gray # [config criteria]
      ),
      text = ggplot2::element_text(
        size = text_size, # [config criteria]
        color = common_gray # [config criteria]
      ),
      axis.text.y = ggplot2::element_text(
        size = text_size, # [config criteria]
        color = common_gray # [config criteria]
      ),
      axis.text.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_text(
        size = text_size, # [config criteria]
        color = common_gray # [config criteria]
      ),
      axis.title.x = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      line = ggplot2::element_line(
        linewidth = line_size, # [config criteria]
        color = common_gray # [config criteria]
      ),
      strip.background = ggplot2::element_rect(
        fill = "white",
        color = common_gray # [config criteria]
      ),
      strip.text = ggplot2::element_blank()
    ) +
    ggplot2::geom_vline(
      xintercept = seq(0, 10e3, time_step * 1e3),
      col = colorspace::lighten(common_gray, amount = 0.5), # [config criteria]
      linetype = 1,
      alpha = 0.5,
      linewidth = line_size # [config criteria]
    )

  plot_line <- function(data_source, sel_col = "black", show_axis = FALSE) {
    if (
      isFALSE(show_axis)
    ) {
      p0 <-
        p0 +
        ggplot2::theme(
          axis.title.y = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank()
        )
    }

    p0 +
      ggplot2::geom_line(
        data = data_source,
        linewidth = line_size * 5,
        color = sel_col
      )
  }

  plot_line_by_var <- function(data_source, sel_var, ...) {
    data_source %>%
      dplyr::select(
        age,
        !!sel_var
      ) %>%
      tidyr::pivot_longer(
        cols = -age,
        names_to = "predictor",
        values_to = "value"
      ) %>%
      plot_line(
        data_source = .,
        ...
      )
  }

  plot_density <- function(data_source, sel_col = "black", show_axis = FALSE) {
    if (
      isFALSE(show_axis)
    ) {
      p0 <-
        p0 +
        ggplot2::theme(
          axis.title.y = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank()
        )
    }
    p0 +
      ggplot2::geom_ribbon(
        data = data_source,
        mapping = ggplot2::aes(
          x = age,
          ymax = value,
          ymin = 0
        ),
        orientation = "x",
        color = sel_col,
        fill = colorspace::lighten(sel_col, amount = 0.5),
        linewidth = line_size * 5
      )
  }

  plot_density_by_var <- function(data_source, sel_var, ...) {
    data_source %>%
      dplyr::select(
        age,
        !!sel_var
      ) %>%
      tidyr::pivot_longer(
        cols = -age,
        names_to = "predictor",
        values_to = "value"
      ) %>%
      plot_density(
        data_source = .,
        ...
      )
  }

  data_sub <-
    data_source %>%
    dplyr::filter(dataset_id == sel_example_record)



  fig_importance <-
    data_sub$varpar_summary_table[[1]] %>%
    dplyr::select(
      predictor,
      ratio = i_perc_percent
    ) %>%
    ggplot2::ggplot() +
    ggplot2::geom_bar(
      ggplot2::aes(
        x = 1,
        y = ratio / 100,
        fill = predictor,
        color = predictor,
      ),
      stat = "identity",
      col = NA,
      position = "stack",
      show.legend = TRUE
    ) +
    ggplot2::scale_y_continuous(
      position = "left",
      limits = c(0, 1),
      breaks = seq(0, 1, 0.2),
      oob = scales::squish
    ) +
    ggplot2::scale_fill_manual(
      values = palette_predictors,
      drop = FALSE
    ) +
    ggplot2::scale_colour_manual(
      values = palette_predictors,
      drop = FALSE
    ) +
    ggplot2::guides(
      colour = "none",
      alpha = "none",
      fill = ggplot2::guide_legend(
        title = "Predictors",
        title.position = "top",
        title.theme = ggplot2::element_text(
          size = text_size, # [config criteria]
          colour = common_gray
        ),
        label.theme = ggplot2::element_text(
          size = text_size, # [config criteria]
          colour = common_gray
        ),
        nrow = 2,
        ncol = 2,
        byrow = TRUE
      )
    ) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = element_text(
        size = text_size, # [config criteria]
        colour = common_gray,
      ),
      legend.text = ggplot2::element_text(
        size = text_size, # [config criteria]
        colour = common_gray
      ),
      legend.margin = ggplot2::margin(
        t = 0,
        r = 0,
        b = 0,
        l = 0,
        unit = "pt"
      ),
      panel.background = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(
        fill = "transparent",
        colour = NA
      ),
      axis.title = ggplot2::element_text(
        size = text_size, # [config criteria]
        colour = common_gray
      ),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_blank(),
      text = ggplot2::element_text(
        size = text_size, # [config criteria]
        colour = common_gray
      ),
      line = ggplot2::element_line(
        linewidth = line_size, # [config criteria]
        colour  = common_gray
      )
    ) +
    ggplot2::labs(
      y = "Ratio of importance",
      x = NULL
    )

  fig_spd <-
    plot_density_by_var(
      data_source = data_sub$data_merge[[1]],
      sel_var = "SPD",
      sel_col = palette_predictors["human"],
      show_axis = TRUE
    )

  list_fig_climate <-
    c(
      "MAT", "MTCO", "MAPS", "MAPW"
    ) %>%
    purrr::map(
      .f = ~ plot_line_by_var(
        data_source = data_sub$data_merge[[1]],
        sel_var = .x,
        sel_col = palette_predictors["climate"]
      )
    )

  fig_predictors <-
    cowplot::plot_grid(
      fig_spd,
      NULL,
      cowplot::plot_grid(
        plotlist = list_fig_climate,
        nrow = 1,
        labels = c("MAT", "MTCO", "MAPS", "MAPW"),
        label_size = text_size, # [config criteria]
        label_colour = common_gray, # [config criteria]
        hjust = -0.5,
        vjust = 0.5
      ),
      nrow = 1,
      labels = c("SPD", NULL, NULL),
      label_size = text_size, # [config criteria]
      label_colour = common_gray, # [config criteria]
      hjust = -1.5,
      vjust = 0.5,
      rel_widths = c(2, 0.5, 4)
    )


  list_fig_paps_line <-
    c(
      "N0", "N1", "N2",
      "N1 minus N2", "N2 divided by N1", "N1 divided by N0",
      "DCCA axis 1", "ROC"
    ) %>%
    purrr::map(
      .f = ~ plot_line_by_var(
        data_source = data_sub$data_merge[[1]],
        sel_var = .x
      )
    )

  list_fig_paps_density <-
    c("Density diversity", "Density turnover") %>%
    purrr::map(
      .f = ~ plot_density_by_var(
        data_source = data_sub$data_merge[[1]],
        sel_var = .x
      )
    )

  fig_paps <-
    cowplot::plot_grid(
      cowplot::plot_grid(
        plotlist = list_fig_paps_line,
        nrow = 1,
        labels = c(
          "N0", "N1", "N2",
          "N1 minus N2", "N2 divided by N1", "N1 divided by N0",
          "DCCA axis 1", "ROC"
        ),
        label_size = text_size, # [config criteria]
        label_colour = common_gray, # [config criteria]
        hjust = -0.5,
        vjust = 0.5
      ),
      cowplot::plot_grid(
        plotlist = list_fig_paps_density,
        nrow = 1,
        labels = c("Density diversity", "Density turnover"),
        label_size = text_size, # [config criteria]
        label_colour = common_gray, # [config criteria]
        hjust = -0.5,
        vjust = 0.5
      ),
      nrow = 1,
      align = "h",
      rel_widths = c(8, 2)
    )

  cowplot::plot_grid(
    fig_importance,
    NULL,
    cowplot::plot_grid(
      fig_predictors,
      NULL,
      fig_paps,
      nrow = 1,
      align = "h",
      rel_widths = c(5.5, 0.5, 10)
    ),
    nrow = 1,
    rel_widths = c(2, 0.5, 16)
  )
}


#----------------------------------------------------------#
# 4. Plot results -----
#----------------------------------------------------------#

human_example_id <- "14944"

fig_human_example <-
  plot_example_figure(
    data_source = data_to_plot,
    sel_example_record = human_example_id
  )


climate_example_id <- "15394"

fig_climate_example <-
  plot_example_figure(
    data_source = data_to_plot,
    sel_example_record = climate_example_id
  )

fig_example_merged <-
  cowplot::plot_grid(
    fig_human_example,
    fig_climate_example,
    nrow = 2,
    rel_heights = c(1, 1),
    labels = "AUTO"
  )

#----------------------------------------------------------#
# 5. Save -----
#----------------------------------------------------------#

purrr::walk(
  .x = c("png", "pdf"),
  .f = ~ ggplot2::ggsave(
    paste(
      here::here(
        "Outputs/Figures/Extended_data_figures/Extended_data_figure_4"
      ),
      .x,
      sep = "."
    ),
    plot = fig_example_merged,
    width = image_width_vec["3col"], # [config criteria]
    height = 220,
    units = image_units, # [config criteria]
    bg = "white"
  )
)
