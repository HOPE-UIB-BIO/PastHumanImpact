#' @title Diagrams or figures on a map 
#' @description Plot selected diagrams or figures on a map using the ggimage package
#' @param data_to_map the tibble with data of coordinates of the subplots
#' @param  subview_plot select which type of plot or figure to map
#' @param fill_background_map colour for the background (i.e oceans)
#' @param colour_map_line colour for the border lines
#' @param colour_map_line colour the borders of countries (? check)
#' @param width_subplot size width of subplots
#' @param height_subplot size height of subplots
#' @return A ggmap with selected figures distributed globally with the coordinates in data_to_map 

plot_figures_on_map <- function(data_to_map,
                                #subview_plot = "euler_diagram", 
                                fill_background_map = "navy",
                                colour_map_line = "grey70",
                                fill_colour_map = "grey80",
                                linewidth_map = 0.5,
                                ...) {
  
  

  
  # set theme map
  maptheme <- ggplot2::theme(
    panel.background = element_rect(
      fill = fill_background_map,
      colour = fill_colour_map,
      linewidth = linewidth_map, 
      linetype = "solid"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  )
  # get world map
  worldmap <- map_data("world")
  
  # create map
  basemap <- worldmap %>%
    ggplot(mapping = ggplot2::aes(
      x = long, 
      y = lat, 
      group = group)
      )  +
    ggplot2::borders(
      colour = NA, 
      fill = fill_colour_map) +
    ggplot2::coord_fixed() +
    maptheme +
    labs(
      x = "Longitudes", 
      y = "Latitudes"
      )
  
  # add selected plot with geom_subview
  fig_on_map <- basemap + 
    ggimage::geom_subview(mapping = ggplot2::aes(
    x = long, 
    y = lat, 
    width = width_subplot, 
    height = width_subplot, 
    subview = subview_figs), 
    data = data_to_map
    )
  
  return(fig_on_map)
}

