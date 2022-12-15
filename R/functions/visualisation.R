
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

# circular plots 
plot_circular_plot <- function(x) {
  
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
  
  
get_plot_euler <- function(data_for_euler) {
  
  v <- venneuler::venneuler(data_for_euler)
  
  newdf <- data.frame(v$centers, 
                      diameters = v$diameters, 
                      predictors = v$labels, 
                      stringsAsFactors = FALSE)
  
  
  plt <- newdf %>%
    mutate(r = diameters/2) %>%
    ggplot() +
    geom_circle(aes(x0 = x, y0 = y, r = r, fill = predictors), alpha = .5) +
    #geom_text(aes(x = x, y = y, label = predictors)) +
    coord_fixed() +
    # theme(legend.position = "none") +
    theme_void() 
  
  plt
  
}   
  
  