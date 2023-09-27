########################################################
## VISUALISATION
## FIGURE 1: RESULTS H1
########################################################

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

data_for_plotting <-
  input_spatial %>%
  dplyr::inner_join(
    input_temporal,
    by = "region"
  )

## CREATE FIGURES

# need to fix issue with dropping unused levels all should be from 500 - 8500

get_predictor_barplot_for_all_regions <- function(
    data_source,
    sel_predictor, sel_palette,
    axis_to_right = TRUE) {
  data_bars <-
    data_source %>%
    tidyr::unnest(data_temporal) %>%
    tidyr::complete(
      age,
      tidyr::nesting(predictor, region),
      fill = list(percentage_median = 0)
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

# barplot climate
list_bars_plot_climate <-
  get_predictor_barplot_for_all_regions(
    data_source = data_for_plotting,
    sel_predictor = "climate",
    sel_palette = palette_predictors,
    axis_to_right = TRUE
  )
# barplot human
list_bars_plot_human <-
  get_predictor_barplot_for_all_regions(
    data_source = data_for_plotting,
    sel_predictor = "human",
    sel_palette = palette_predictors,
    axis_to_right = FALSE
  )

# circular bars
list_circle_plots <-
  data_for_plotting %>%
  tidyr::unnest(data_spatial) %>%
  dplyr::group_by(region) %>%
  dplyr::group_map(
    .f = ~ get_circular_barplot(
      data = .x,
      y_var = "percentage_median",
      x_var = "predictor",
      col_vec = palette_ecozones, # [config criteria]
      x_name = predictors_label # [config criteria]
    )
  ) %>%
  rlang::set_names(
    nm = unique(data_for_plotting$region)
  )


# ggsave(
#   "C:/Users/vfe032/Documents/Github/HOPE_Hypothesis1/cir.png",
#   plot = list_circle_plots,
#   width = 20,
#   units = "mm",
#   dpi = 700,
#   bg = "transparent"
#
# )

combine_circle_and_bars <- function(
    data_source_plot_circe = list_circle_plots,
    data_source_plot_climate = list_bars_plot_climate,
    data_source_plot_human = list_bars_plot_human,
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
        data_source_plot_circe[[sel_region]],
        data_source_plot_human[[sel_region]],
        nrow = 1,
        ncol = 3,
        widths = c(0.2, 1, 0.2),
      )
    return(combined_fig)
  }
}

combine_circle_and_bars(sel_region = "North America")




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


# INSET FIGURES ON MAP
combined_map_h1 <-
  cowplot::ggdraw(worldmap_grey) +
  cowplot::draw_plot(fig1,
    x = 0.05,
    y = 0.4,
    width = .3,
    height = .45
  ) +
  cowplot::draw_plot(fig2,
    x = 0.18,
    y = 0.18,
    width = .3,
    height = .45
  ) +
  cowplot::draw_plot(fig3,
    x = 0.36,
    y = 0.43,
    width = .3,
    height = .45
  ) +
  cowplot::draw_plot(fig4,
    x = 0.68,
    y = 0.4,
    width = .3,
    height = .45
  ) +
  cowplot::draw_plot(fig5,
    x = 0.65,
    y = 0.15,
    width = .3,
    height = .45
  )


combined_map_h1
# For guidance, Nature’s standard figure sizes are 90 mm (single column) and 180 mm (double column) and the full depth of the page is 170 mm.


ggsave(
  "combined_map_h1.png",
  plot = combined_map_h1,
  width = 70,
  height = 60,
  units = "mm",
  bg = "white"
)







#
#
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
