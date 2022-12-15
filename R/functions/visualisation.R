
# SET DEFAULT FIGU PARAMETERS
#----

# VISUALISATION FUNCTIONS

# barplot of predictors mean adjusted R2 uniquely explained or individually explained
plot_individual_predictor_bars <- function(x,
                                           y = "mean_unique_adj_r2",
                                           group_vars = c("region",
                                                          "ecozone_koppen_5",
                                                          "predictor")) {
  x  %>% 
    group_by(across(all_of(group_vars))) %>%
    summarise(
      mean_unique_adj_r2 = mean(Unique),
      mean_ind_adj_r2 = mean(Individual),
      n = n()
    ) %>% 
    ungroup() %>% 
    ggplot(aes(y = get(y), 
               x = reorder(predictor, - get(y)), 
               fill = predictor)) +
    geom_bar(stat = "identity", 
             alpha = 0.5) +
    facet_wrap(~region + ecozone_koppen_5) +
    labs(x = "Predictor variables", y = "Mean adjusted r2")
}

# boxplot of explained variation in ecozones within regions
boxplot_expvar_by_region <- function(x) {
  x %>%
    ggplot(aes(y = AdjR2_total, 
               x = ecozone_koppen_5, 
               fill = ecozone_koppen_5)) +
    geom_boxplot() +
    facet_wrap(~region) +
    labs(x = "Ecozones", y = "Explained adjusted R2")
}



#euler diagrams
get_data_for_euler <- function(data_source) {
  
  rep_string <- c("Unique to human" = "Human",
                  "Unique to climate" = "Climate",
                  "Unique to time" = "Time",
                  "Common to human, and climate" = "Human&Climate",
                  "Common to human, and time" = "Human&Time",
                  "Common to climate, and time" = "Climate&Time",
                  "Common to human, climate, and time" = "Human&Climate&Time")
  
  
  data_work <-
    as.data.frame(data_source) %>%
    tibble::rownames_to_column("labels") %>%
    tibble::as_tibble() %>%
    mutate(labels = stringr::str_replace_all(labels, rep_string),
           labels = stringr::str_replace_all(labels, " ", "")) %>%
    filter(!labels == "Total") %>%
    
    dplyr::pull(Fractions, labels) %>%
    return()
}
  
  
get_plot_euler <- function(data_for_euler, alpha = .7) {
  
  v <- venneuler::venneuler(data_for_euler)
  
  newdf <- data.frame(v$centers, 
                      diameters = v$diameters, 
                      predictors = v$labels, 
                      stringsAsFactors = FALSE) %>%
    mutate(r = diameters/2)
    
  
  
  plt <- newdf %>%
    ggplot() +
    ggforce::geom_circle(aes(x0 = x, y0 = y, r = r, fill = predictors), colour = NA, alpha = alpha) +
    coord_fixed() +
    theme_void() +
    theme(legend.position = "none") +
    theme_transparent()
  
  plt
  
}   


get_euler_on_map <- function(source_data, data_meta) {
  
  require(ggimage)
  
  euler_data <- source_data %>% 
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

  