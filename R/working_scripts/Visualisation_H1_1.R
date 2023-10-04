#----------------------------------------------------------#
#
#
#                   GlobalHumanImpact
#
#                    VISUALISATION
#                 FIGURE 1: RESULTS H1
#
#                   V. Felde, O. Mottl
#                         2023
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

# Import tables for plotting

source(
  here::here(
    "R/working_scripts/Results_script.R"
  )
)

# helper functions
get_predictor_barplot_for_all_regions <- function(
    data_source,
    sel_predictor, sel_palette,
    axis_to_right = TRUE) {
  data_bars <-
    data_source %>%
    tidyr::unnest(data_temporal) %>%
    tidyr::complete(
      age,
      tidyr::nesting(predictor, region)
    ) %>%
    dplyr::mutate(
      no_data = ifelse(is.na(percentage_median), TRUE, FALSE)
    ) %>%
    dplyr::mutate(
      percentage_median = ifelse(no_data, 0, percentage_median)
    ) %>%
    dplyr::group_by(region) %>%
    tidyr::nest(data_to_plot = -c(region)) %>%
    dplyr::mutate(
      plot = purrr::map(
        .x = data_to_plot,
        .f = ~ get_predictor_barplot(
          data = .x,
          sel_predictor = sel_predictor,
          x_var = "percentage_median",
          axis_to_right = axis_to_right,
          sel_palette = sel_palette
        )
      )
    )

  data_bars %>%
    purrr::chuck("plot") %>%
    rlang::set_names(
      nm = data_bars$region
    ) %>%
    return()
}

plot_dist_density <- function(
    data_source, sel_predictor,
    text_size = 6,
    axis_to_right = TRUE) {
  data_work <-
    dplyr::filter(
      data_source, predictor == sel_predictor
    )

  fig <-
    data_work %>%
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = percentage
      )
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_colour_manual(
      values = palette_predictors_parts # [config criteria]
    ) +
    ggplot2::scale_fill_manual(
      values = palette_predictors_parts # [config criteria]
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      text = ggplot2::element_text(
        size = text_size
      ),
      line = ggplot2::element_line(
        linewidth = 0.01 # [config criteria]
      ),
      legend.position = "none",
      plot.margin = grid::unit(c(0, 0, 0, 0), "mm"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(
        fill = "transparent", color = NA
      ),
      plot.background = ggplot2::element_rect(
        fill = "transparent", color = NA
      ),
      panel.border = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.ticks.length = grid::unit(0, "mm")
    ) +
    ggplot2::labs(
      x = "Explained variability (%)",
      y = "Number of records"
    ) +
    ggplot2::geom_density(
      mapping = ggplot2::aes(
        y = after_stat(count),
        col = var_part,
        fill = var_part
      ),
      alpha = 0.4,
      linewidth = 0.1
    )

  if (isTRUE(axis_to_right)) {
    fig <-
      fig +
      ggplot2::scale_x_continuous(
        name = NULL,
        limits = c(0, 100),
        breaks = seq(0, 100, by = 25),
        expand = c(0, 0),
        position = "top"
      ) +
      ggplot2::scale_y_continuous(
        limits = c(0, NA)
      )
  } else {
    fig <-
      fig +
      ggplot2::scale_x_continuous(
        name = NULL,
        limits = c(0, 100),
        breaks = seq(0, 100, by = 25),
        expand = c(0, 0),
        position = "bottom"
      ) +
      ggplot2::scale_y_continuous(
        trans = "reverse",
        limits = c(NA, 0)
      )
  }

  return(fig)
}

combine_circle_and_bars <- function(
    data_source_plot_circe = list_circle_plots,
    data_source_plot_climate = list_bars_plot_climate,
    data_source_plot_human = list_bars_plot_human,
    data_source_plot_density_human = list_density_plots_human,
    data_source_plot_density_climate = list_density_plots_climate,
    sel_region,
    sel_method = c("patchwork", "cowplot", "ggpubr")) {
  sel_method <- match.arg(sel_method)

  if (
    sel_method == "patchwork"
  ) {
    # # layout
    layout <- c(
      patchwork::area(t = 2, l = 1, b = 2, r = 1),
      patchwork::area(t = 1, l = 2, b = 3, r = 4),
      patchwork::area(t = 2, l = 5, b = 2, r = 5)
    )

    # combine test 1 patchwork and layout
    combined_fig <-
      data_source_plot_climate[[sel_region]] +
      data_source_plot_circe[[sel_region]] +
      data_source_plot_human[[sel_region]] +
      patchwork::plot_layout(design = layout) +
      ggpubr::theme_transparent() +
      ggplot2::theme(
        aspect.ratio = 2,
        plot.margin = grid::unit(c(0, 0, 0, 0), "mm")
      )

    return(combined_fig)
  }

  if (
    sel_method == "cowplot"
  ) {
    combined_fig <-
      cowplot::ggdraw(data_source_plot_circe[[sel_region]]) +
      cowplot::draw_plot(
        data_source_plot_climate[[sel_region]] +
          ggplot2::theme(aspect.ratio = 5),
        x = 0.01,
        y = 0.15,
        width = .2,
        height = .6
      ) +
      cowplot::draw_plot(
        data_source_plot_human[[sel_region]] +
          ggplot2::theme(aspect.ratio = 5),
        x = 0.8,
        y = 0.15,
        width = .2,
        height = .6
      ) +
      ggplot2::theme(aspect.ratio = 0.85)

    return(combined_fig)
  }

  if (
    sel_method == "ggpubr"
  ) {
    combined_fig <-
      ggpubr::ggarrange(
        data_source_plot_climate[[sel_region]],
        data_source_plot_density_climate[[sel_region]],
        data_source_plot_circe[[sel_region]],
        data_source_plot_density_human[[sel_region]],
        data_source_plot_human[[sel_region]],
        nrow = 1,
        ncol = 5,
        widths = c(0.3, 0.3, 1.5, 0.3, 0.3)
      )
    return(combined_fig)
  }
}


#----------------------------------------------------------#
# 1. Data Wrangling -----
#----------------------------------------------------------#
# Combine output tables of spatial and temporal data
input_spatial <-
  summary_spatial_median %>%
  dplyr::mutate(
    sel_classification = factor(sel_classification)
  ) %>%
  dplyr::mutate(
    predictor = factor(
      predictor,
      levels = predictors_spatial_order # [config criteria]
    )
  ) %>%
  dplyr::filter(n_records > 5) %>%
  tidyr::nest(data_spatial = -c(region))

input_temporal <-
  summary_temporal_median %>%
  dplyr::mutate(
    predictor = factor(predictor,
      levels = c(
        "human",
        "climate"
      )
    )
  ) %>%
  tidyr::nest(data_temporal = -c(region))

data_dist <-
  data_spatial_vis %>%
  dplyr::mutate(
    predictor = factor(
      predictor,
      levels = c("human", "climate", "time")
    )
  ) %>%
  dplyr::mutate(
    sel_classification = factor(sel_classification)
  ) %>%
  tidyr::pivot_longer(
    c(unique_percent, average_share_percent, individual_percent),
    names_to = "var_part",
    values_to = "percentage"
  ) %>%
  dplyr::mutate(
    var_part = factor(var_part,
      levels = c(
        "unique_percent",
        "average_share_percent",
        "individual_percent"
      )
    )
  )

data_for_plotting <-
  dplyr::inner_join(
    input_spatial,
    input_temporal,
    by = "region"
  )

#----------------------------------------------------------#
# 2. Figuress -----
#----------------------------------------------------------#

# 2.1 map -----

# get basemap
world <- map_data("world")

# grey basemap
worldmap_grey <-
  world %>%
  filter(lat > -60 & lat < 85) %>%
  ggplot() +
  geom_polygon(
    aes(
      x = long,
      y = lat,
      group = group
    ),
    fill = "grey90",
    alpha = 0.4
  ) +
  coord_equal(ratio = 1.3) +
  ggplot2::theme_void()


# 2.2 barplots -----
# need to fix issue with dropping unused levels all should be from 500 - 8500

# barplot climate
list_bars_plot_climate <-
  get_predictor_barplot_for_all_regions(
    data_source = data_for_plotting,
    sel_predictor = "climate",
    sel_palette = palette_predictors,
    axis_to_right = FALSE
  )

# barplot human
list_bars_plot_human <-
  get_predictor_barplot_for_all_regions(
    data_source = data_for_plotting,
    sel_predictor = "human",
    sel_palette = palette_predictors,
    axis_to_right = TRUE
  )

# 2.3 circular plot -----

data_circle_plots <-
  data_for_plotting %>%
  tidyr::unnest(data_spatial) %>%
  dplyr::group_by(region) %>%
  tidyr::nest(data_to_plot = -c(region)) %>%
  dplyr::mutate(
    plot = purrr::map(
      .x = data_to_plot,
      .f = ~ get_circular_barplot(
        data = .x,
        y_var = "percentage_median",
        x_var = "predictor",
        col_vec = palette_ecozones, # [config criteria]
        x_name = predictors_label # [config criteria]
      )
    )
  )

list_circle_plots <-
  data_circle_plots %>%
  purrr::chuck("plot") %>%
  rlang::set_names(
    nm = unique(data_circle_plots$region)
  )

# 2.4 Denisty plots -----

# Density figures for full distribution of variance
data_density_plot <-
  data_dist %>%
  dplyr::group_by(region) %>%
  tidyr::nest(data_dist = -c(region)) %>%
  dplyr::mutate(
    plot_human = purrr::map(
      .x = data_dist,
      .f = ~ plot_dist_density(
        data_source = .x,
        sel_predictor = "human",
        axis_to_right = FALSE
      )
    ),
    plot_climate = purrr::map(
      .x = data_dist,
      .f = ~ plot_dist_density(
        data_source = .x,
        sel_predictor = "climate",
        axis_to_right = TRUE
      )
    )
  )

list_density_plots_human <-
  data_density_plot %>%
  purrr::chuck("plot_human") %>%
  rlang::set_names(
    nm = unique(data_density_plot$region)
  )

list_density_plots_climate <-
  data_density_plot %>%
  purrr::chuck("plot_climate") %>%
  rlang::set_names(
    nm = unique(data_density_plot$region)
  )

# 2.5 INSET FIGURES ON MAP -----

sel_method <- "ggpubr"
sel_figure_width <- 0.3
sel_figure_height <- 0.35

combined_map_h1 <-
  cowplot::ggdraw(worldmap_grey) +
  cowplot::draw_plot(
    combine_circle_and_bars(
      sel_region = "North America", sel_method = sel_method
    ),
    x = 0.05,
    y = 0.5,
    width = sel_figure_width,
    height = sel_figure_height
  ) +
  cowplot::draw_plot(
    combine_circle_and_bars(
      sel_region = "Latin America", sel_method = sel_method
    ),
    x = 0.20,
    y = 0.10,
    width = sel_figure_width,
    height = sel_figure_height
  ) +
  cowplot::draw_plot(
    combine_circle_and_bars(
      sel_region = "Europe", sel_method = sel_method
    ),
    x = 0.35,
    y = 0.5,
    width = sel_figure_width,
    height = sel_figure_height
  ) +
  cowplot::draw_plot(
    combine_circle_and_bars(
      sel_region = "Asia", sel_method = sel_method
    ),
    x = 0.65,
    y = 0.5,
    width = sel_figure_width,
    height = sel_figure_height
  ) +
  cowplot::draw_plot(
    combine_circle_and_bars(
      sel_region = "Oceania", sel_method = sel_method
    ),
    x = 0.55,
    y = 0.10,
    width = sel_figure_width,
    height = sel_figure_height
  )

combined_map_h1_with_space_for_legend <-
  cowplot::plot_grid(
    combined_map_h1,
    patchwork::plot_spacer(),
    nrow = 2,
    rel_heights = c(3, 1)
  )

# 2.6 save -----
purrr::walk(
  .x = c("png", "pdf"),
  .f = ~ ggplot2::ggsave(
    paste(
      here::here("Outputs/combined_map_h1"),
      .x,
      sep = "."
    ),
    plot = combined_map_h1_with_space_for_legend,
    width = image_width_vec["2col"],
    height = 100, # 85
    units = "mm",
    bg = "white"
  )
)

#----------------------------------------------------------#
# ## function
# get_combine_figure <- function(input_temporal,
#                                input_spatial) {
#
#
#    list_bars_plot_climate <-
#     get_predictor_barplot(
#       data = input_temporal,
#       sel_predictor = "climate",
#       x_var = "percentage_median",
#       axis_to_right = TRUE)
#
#
#    list_bars_plot_human <-
#      get_predictor_barplot(
#        data = input_temporal,
#        sel_predictor = "human",
#        x_var = "percentage_median",
#        axis_to_right = FALSE)
#
#    list_circle_plots <-
#      get_circular_barplot(
#        data = input_spatial,
#        y_var = "percentage_median",
#        x_var = "predictor",
#        fill_var = "sel_classification",
#        col_vec = palette_ecozones,
#        x_name = x_label)
#
#    combine <-
#      cowplot::ggdraw(list_circle_plots) +
#
#      cowplot::draw_plot(list_bars_plot_climate,
#                         x = 0.01,
#                         y = 0.15,
#                         width = .2,
#                         height = .6) +
#      cowplot::draw_plot(bars_human,
#                         x = 0.8,
#                         y = 0.15,
#                         width = .2,
#                         height = .6) +
#      theme(aspect.ratio = 0.7,
#            plot.margin = unit(c(0,0,0,0), "mm")
#      )
#
#    return(combine)
#
#
# }
#
# plotting <-
#   data_for_plotting %>%
#   mutate(figs = purrr::map2(
#     .x = data_spatial,
#     .y = data_temporal,
#     .f = ~get_combine_figure(
#       input_temporal = .y,
#       input_spatial = .x
#     )
#   )
#   )
#
#
#
#
# ### GET FIGURES TO MAP
#
# #regional limits
# region_plot <-
#   tibble(
#   region = c("North America", "Latin America", "Europe", "Asia",  "Oceania"),
#   x = c(-110, -70, 10, 100, 125),
#   y = c(50, -20, 50, 65, -25),
#   width = rep(100, 5)
# )
#
# # add combine figs
# plotting <-
#   data_for_plotting %>%
#   mutate(figs = purrr::map2(
#     .x = data_spatial,
#     .y = data_temporal,
#     .f = ~get_combine_figure(
#       input_temporal = .y,
#       input_spatial = .x
#     )
#   )
#   )
#
# region_plot <-
#   region_plot %>%
#   inner_join(plotting) %>%
#   dplyr::select(-c(data_spatial, data_temporal))
#
# #get basemap
# world <- map_data("world")
#
# # grey basemap
# worldmap_grey <-
#   world %>%
#   filter(lat > -60 & lat < 85) %>%
#   ggplot() +
#   geom_polygon(
#     aes(x = long,
#         y = lat,
#         group = group),
#     fill = "grey90",
#     alpha = 0.4
#   ) +
#   coord_equal(ratio = 1.3) +
#   theme_void()
#
#
# fig_on_map <-
#  worldmap_grey +
#   ggimage::geom_subview(
#     mapping = ggplot2::aes(
#     x = x,
#     y = y,
#     width = width,
#     height = width,
#     subview = figs),
#     data = region_plot
#   )
#
# fig_on_map
#
#
