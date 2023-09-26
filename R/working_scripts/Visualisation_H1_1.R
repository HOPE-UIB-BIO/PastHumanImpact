########################################################
## VISUALISATION
## FIGURE 1: RESULTS H1
########################################################

# Import tables for plotting

source("R/working_scripts/Results_script.R")



# Define colour palette
# Ecozones
palette_ecozones <-
  c(
    Polar = "#009292",
    Cold_Without_dry_season_Very_Cold_Summer = "#004949",
    Cold_Without_dry_season_Cold_Summer = "#006ddb",
    Cold_Dry_Winter = "#6db6ff",
    Cold_Dry_Summer = "#b6dbff",
    Cold_Without_dry_season_Warm_Summer = "#117733",
    Cold_Without_dry_season_Hot_Summer = "#999933",
    Temperate_Without_dry_season = "#DDCC77",
    Temperate_Dry_Winter = "#b66dff",
    Temperate_Dry_Summer = "#ffff6d",
    Arid = "#924900",
    Tropical = "#920000"
  )

# Predictors
palette_pred <- c(
  human = "#663333",
  climate = "#BBBBBB"
)

# Parameters
order_predictors_spatial <- c("human", "time", "climate")
x_label <- c("Human", "Time", "Climate")

# Combine output tables of spatial and temporal data
input_spatial <-
  summary_spatial_median %>%
  mutate(sel_classification = factor(sel_classification)) %>%
  mutate(predictor = factor(predictor,
    levels = order_predictors_spatial
  )) %>%
  filter(n_records > 5) %>%
  nest(data_spatial = -c(region))


input_temporal <-
  summary_temporal_median %>%
  mutate(predictor = factor(predictor,
    levels = c(
      "human",
      "climate"
    )
  )) %>%
  nest(data_temporal = -c(region))


data_for_plotting <-
  input_spatial %>%
  inner_join(input_temporal,
    by = "region"
  )



## CREATE FIGURES

# need to fix issue with dropping unused levels all should be from 500 - 8500

# barplot climate
bars_climate <-
  data_for_plotting %>%
  unnest(data_temporal) %>%
  tidyr::complete(age,
    nesting(predictor, region),
    fill = list(percentage_median = 0)
  ) %>%
  group_by(region) %>%
  group_map(~ get_predictor_barplot(
    data = .x,
    sel_predictor = "climate",
    x_var = "percentage_median",
    axis_to_right = TRUE
  ))

names(bars_climate) <- data_for_plotting$region %>% unique()


# barplot human
bars_human <-
  data_for_plotting %>%
  unnest(data_temporal) %>%
  tidyr::complete(age,
    nesting(predictor, region),
    fill = list(percentage_median = 0)
  ) %>%
  group_by(region) %>%
  group_map(~ get_predictor_barplot(
    data = .x,
    sel_predictor = "human",
    x_var = "percentage_median",
    axis_to_right = FALSE
  ))


names(bars_human) <- data_for_plotting$region %>% unique()

# circular bars
bars_circ <-
  data_for_plotting %>%
  unnest(data_spatial) %>%
  group_by(region) %>%
  group_map(~ get_circular_barplot(
    data = .x,
    y_var = "percentage_median",
    x_var = "predictor",
    col_vec = palette_ecozones,
    x_name = x_label
  ))


names(bars_circ) <- data_for_plotting$region %>% unique()





# ggsave(
#   "C:/Users/vfe032/Documents/Github/HOPE_Hypothesis1/cir.png",
#   plot = bars_circ,
#   width = 20,
#   units = "mm",
#   dpi = 700,
#   bg = "transparent"
#
# )


# # layout
layout <- c(
  area(t = 2, l = 1, b = 2, r = 1),
  area(t = 1, l = 2, b = 3, r = 4),
  area(t = 2, l = 5, b = 2, r = 5)
)

# combine test 1 patchwork and layout
combined_fig <-
  bars_climate$Europe + bars_circ$Europe + bars_human$Europe +
  plot_layout(design = layout) +
  theme_transparent +
  theme(
    aspect.ratio = 2,
    plot.margin = grid::unit(c(0, 0, 0, 0), "mm")
  )

combined_fig

# combine plots per region

fig1 <-
  cowplot::ggdraw(bars_circ$`North America`) +
  cowplot::draw_plot(
    bars_climate$`North America` +
      theme(aspect.ratio = 5),
    x = 0.01,
    y = 0.15,
    width = .2,
    height = .6
  ) +
  cowplot::draw_plot(
    bars_human$`North America` +
      theme(aspect.ratio = 5),
    x = 0.8,
    y = 0.15,
    width = .2,
    height = .6
  ) +
  theme(aspect.ratio = 0.85)



fig2 <-
  cowplot::ggdraw(bars_circ$`Latin America`) +
  cowplot::draw_plot(
    bars_climate$`Latin America` +
      theme(aspect.ratio = 5),
    x = 0.01,
    y = 0.15,
    width = .2,
    height = .6
  ) +
  cowplot::draw_plot(
    bars_human$`Latin America` +
      theme(aspect.ratio = 5),
    x = 0.8,
    y = 0.15,
    width = .2,
    height = .6
  ) +
  theme(aspect.ratio = 0.85)


fig3 <-
  cowplot::ggdraw(bars_circ$Europe) +
  cowplot::draw_plot(
    bars_climate$Europe +
      theme(aspect.ratio = 5),
    x = 0.01,
    y = 0.15,
    width = .2,
    height = .6
  ) +
  cowplot::draw_plot(
    bars_human$Europe +
      theme(aspect.ratio = 5),
    x = 0.8,
    y = 0.15,
    width = .2,
    height = .6
  ) +
  theme(aspect.ratio = 0.85)


fig4 <-
  cowplot::ggdraw(bars_circ$Asia) +
  cowplot::draw_plot(
    bars_climate$Asia +
      theme(aspect.ratio = 5),
    x = 0.01,
    y = 0.15,
    width = .2,
    height = .6
  ) +
  cowplot::draw_plot(
    bars_human$Asia +
      theme(aspect.ratio = 5),
    x = 0.8,
    y = 0.15,
    width = .2,
    height = .6
  ) +
  theme(aspect.ratio = 0.85)

fig5 <-
  cowplot::ggdraw(bars_circ$Oceania) +
  cowplot::draw_plot(
    bars_climate$Oceania +
      theme(aspect.ratio = 5),
    x = 0.0,
    y = 0.15,
    width = .2,
    height = .6
  ) +
  cowplot::draw_plot(
    bars_human$Oceania +
      theme(aspect.ratio = 5),
    x = 0.9,
    y = 0.15,
    width = .2,
    height = .6
  ) +
  theme(aspect.ratio = 0.85)




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
  theme_void()


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
#    bars_climate <-
#     get_predictor_barplot(
#       data = input_temporal,
#       sel_predictor = "climate",
#       x_var = "percentage_median",
#       axis_to_right = TRUE)
#
#
#    bars_human <-
#      get_predictor_barplot(
#        data = input_temporal,
#        sel_predictor = "human",
#        x_var = "percentage_median",
#        axis_to_right = FALSE)
#
#    bars_circ <-
#      get_circular_barplot(
#        data = input_spatial,
#        y_var = "percentage_median",
#        x_var = "predictor",
#        fill_var = "sel_classification",
#        col_vec = palette_ecozones,
#        x_name = x_label)
#
#    combine <-
#      cowplot::ggdraw(bars_circ) +
#
#      cowplot::draw_plot(bars_climate,
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
