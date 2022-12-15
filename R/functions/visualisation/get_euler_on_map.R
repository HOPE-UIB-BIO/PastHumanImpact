#' @title Euler diagrams on a map
#' @description Plot Euler diagrams on a map
#' @param data_source Named list with hierarchical variation partitioning results
#' @param data_meta Tibble with meta data
#' @return A ggmap with Euler diagrams

get_euler_on_map <- function(data_source, data_meta) {
  
  require(ggimage)
  
  euler_data <- data_source %>% 
    purrr::map(pluck(1)) %>%
    purrr::map(pluck("Var.part")) %>%
    purrr::map(get_data_for_euler)
  
  data_to_map <- 
    tibble(dataset_id = names(euler_data), 
           as_tibble(lst(euler_data))) %>%
    inner_join(data_meta %>% 
                 dplyr::select(dataset_id, lat, long), by = "dataset_id") %>%
    mutate(euler_plot = purrr::map(euler_data,.f = get_plot_euler)) %>%
    mutate(width = 15)
  
  # set theme map
  maptheme <- theme(
    panel.background = element_rect(fill = "navy",
                                    colour = "grey70",
                                    linewidth = 0.5, linetype = "solid"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  )
  # get world map
  worldmap <- map_data("world")
  
  # create map
  mapplot1 <- worldmap %>%
    ggplot(aes(x = long, y = lat, group = group))  +
    borders(colour = NA, fill = "grey80") +
    coord_fixed() +
    maptheme +
    labs(x = "Longitudes", y = "Latitudes")
  
  # add euler plot with geom_subview
  mapplot1 + geom_subview(aes(x = long, y = lat, width = width, height = width, subview = euler_plot), data = data_to_map)
  
  return(mapplot1)
}
