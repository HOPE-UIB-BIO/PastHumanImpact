
# get temporal barcharts
get_temporal_barcharts <- function(data) {
  
 p <- data %>%
    ggplot() +
    geom_bar(data = data  %>% 
               dplyr::filter(variance_partition == "Unique_percent_median"), 
             aes(x = as.factor(age),
                 y = percentage_median,
                 fill = predictor
             ),
             stat = "identity",
             width = 1,
             alpha = 1, 
             show.legend = FALSE) +
    geom_bar(data = data %>% 
               dplyr::filter(variance_partition == "Average.share_percent_median"),
             aes(x = as.factor(age),
                 y = percentage_median,
                 fill = predictor),
             stat = "identity",
             width = 1,   
             alpha = 0.4, 
             show.legend = FALSE) +
    scale_fill_manual(values = palette_pred) +
    scale_y_continuous(name = NULL, 
                      limits = c(0, 105),
                      breaks = seq(0, 105, by = 20),
                      expand = c(0,0)) +
    facet_wrap(~ predictor, strip.position = "bottom", scales = "free_x") +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 8),
      axis.text.x = element_text(size = 8, angle = 60, hjust = 1),
      legend.position = "none", 
      plot.margin = unit(c(0.2, 0.2, 0, 0.1), "cm"),
      strip.background = element_blank(),
      strip.placement = "outside",
      strip.text = element_text(size = 11)
    ) +
    labs(x = "")
 p
 
}


