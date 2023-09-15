# SUMMARY FIGURE FOR H1





# data
input_spatial <- 
  summary_spatial_median %>% 
  mutate(sel_classification = factor(sel_classification)) %>%
  mutate(predictor = factor(predictor, 
                            levels = order_predictors_spatial)) %>%
  filter(n_records > 5) %>%
  nest(data_spatial = -c(region))
  

input_temporal <- 
  summary_temporal_median %>%
  #filter(region %in% select_region) %>%
  mutate(predictor =  factor(predictor, 
                             levels = c("human", 
                                        "climate"))) %>%
  nest(data_temporal = -c(region))


data_for_plotting <- 
  input_spatial %>%
  inner_join(input_temporal, by = "region")





theme_transparant <-
  ggplot2::theme(
    panel.background = element_rect(fill = "transparent",
                                    colour = NA),
    plot.background = element_rect(fill = "transparent",
                                   colour = NA)
  ) 

theme_aspect <- 
  ggplot2::theme(
    aspect.ratio = 5
  )



#figs
bars_climate <- 
  get_predictor_barplot(
    data = data_for_plotting$data_temporal[[1]],
    sel_predictor = "climate",
    x_var = "percentage_median",
    axis_to_right = TRUE) +
  theme_aspect +
  theme_transparant


bars_human <- 
  get_predictor_barplot(
    data = data_for_plotting$data_temporal[[1]],
    sel_predictor = "human",
    x_var = "percentage_median",
    axis_to_right = FALSE) +
  theme_aspect +
  theme_transparant



bars_circ <- 
  get_circular_barplot(
    data = data_for_plotting$data_spatial[[1]],
    y_var = "percentage_median", 
    x_var = "predictor",
    col_vec = palette_ecozones,
    x_name = x_label)


# layout
layout <- c(
  area(t = 2, l = 1, b = 2, r = 1),
  area(t = 1, l = 2, b = 3, r = 4),
  area(t = 2, l = 5, b = 2, r = 5)
)

# combine test 1 patchwork and layout
combined_fig <- 
  bars_climate + cir_plot + bars_human +
  plot_layout(design = layout)
combined_fig

# combine test 2 plot inset
combined_fig2 <-
  cowplot::ggdraw(cir_plot) +
  cowplot::draw_plot(bars_climate, x = 0.01, y = 0.15, width = .2, height = .8) +
  cowplot::draw_plot(bars_human, x = 0.8, y = 0.15, width = .2, height = .8)

combined_fig2

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
#      cowplot::draw_plot(bars_climate, x = 0.01, y = 0.1, width = .2, height = .7) +
#      cowplot::draw_plot(bars_human, x = 0.8, y = 0.1, width = .2, height = .7) + 
#      theme_transparant 
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




### GET FIGURES TO MAP

#regional limits
region_plot <- 
  tibble(
  region = c("North America", "Latin America", "Europe", "Asia",  "Oceania"),
  x = c(-90, -70, 10, 100, 125),
  y = c(50, -20, 50, 65, -25),
  width = rep(100, 5)
)

# add combine figs
plotting <- 
  data_for_plotting %>%
  mutate(figs = purrr::map2(
    .x = data_spatial,
    .y = data_temporal, 
    .f = ~get_combine_figure(
      input_temporal = .y,
      input_spatial = .x
    )
  )
  )

region_plot <- 
  region_plot %>%
  inner_join(plotting) %>%
  dplyr::select(-c(data_spatial, data_temporal))

#get basemap
world <- map_data("world")

# grey basemap
worldmap_grey <- 
  world %>%
  ggplot() +
  geom_polygon(
    aes(x = long,
        y = lat,
        group = group),
    fill = "grey",
    alpha = 0.4
  ) +
  coord_equal() +
  theme_void()


fig_on_map <- 
 worldmap_grey + 
  ggimage::geom_subview(
    mapping = ggplot2::aes(
    x = x, 
    y = y, 
    width = width, 
    height = width, 
    subview = figs), 
    data = region_plot
  )

fig_on_map

 



 
# # alternative test with geom_image
# img <- list.files(system.file("extdata", package="ggimage"),
#                   pattern="png", full.names=TRUE)
# 
# 
# d <- data.frame(x = rnorm(10),
#                 y = rnorm(10),
#                 image = sample(img, size=10, replace = TRUE)
# )
# 
# ggplot(d, aes(x, y)) + geom_image(aes(image=image), size=.05)