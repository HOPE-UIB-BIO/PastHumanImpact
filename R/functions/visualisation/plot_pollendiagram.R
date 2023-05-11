plot_pollendiagram <- function(x, abundance_threshold = 3, increase = 10) {
  plt <- x %>%
    group_by(taxa) %>%
    filter(max(percent)>= abundance_threshold, sum(percent >0) >= abundance_threshold) %>%
    droplevels() %>%
    ungroup() %>%
    mutate(taxa = forcats::fct_reorder(taxa, percent, .fun = "median", .desc = TRUE)) %>%
    ggplot(aes(x = age, y = percent, fill = taxa)) + 
    ggplot2::geom_area(show.legend = FALSE) +  
    ggplot2::geom_area(aes(y = percent*increase), alpha = 0.3, show.legend = FALSE) +
    coord_flip(ylim = c(0, max(x$percent))) +
    scale_x_reverse(breaks = round(seq(min(x$age), max(x$age), by = 1000), 1)) +
    scale_fill_hue(c = 50, l = 50, h = c(30, 300)) +
    theme_classic() +
    facet_grid(~taxa, scales = "free", space = "free") +
    theme(legend.position = "none",
          strip.background = element_blank(),
          panel.spacing = unit(x = 1, units = "pt"),
          plot.margin = unit(c(1, 0, 1, 0), "cm"),
          axis.text.x = element_text(size = 8, angle = 60, hjust = 1),
          strip.text.x = element_text(size = 8, angle = 90,  hjust = 0)) +
    labs(x = "Cal. Median Ages BP", y = "Percentage (%)") 
  
  return(plt)
  
}
