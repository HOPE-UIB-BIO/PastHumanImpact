boxplot_expvar_by_region <- function(x) {
  x %>%
    ggplot(aes(y = AdjR2_total, 
               x = ecozone_koppen_5, 
               fill = ecozone_koppen_5)) +
    geom_boxplot() +
    facet_wrap(~region) +
    labs(x = "Ecozones", y = "Explained adjusted R2")
}