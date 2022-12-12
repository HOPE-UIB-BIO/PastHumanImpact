
# SET DEFAULT FIGU PARAMETERS
#----

# VISUALISATION FUNCTIONS
# PLOT INDIVIDUAL PREDICTOR BARS PER REGION PER ECOZONE
# # barplot of predictors mean adjusted r2 uniquely explained or individually explained
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