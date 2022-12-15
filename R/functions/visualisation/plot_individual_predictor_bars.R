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