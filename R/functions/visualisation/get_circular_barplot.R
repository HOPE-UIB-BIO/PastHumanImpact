# circular barplot

get_circular_barplot <- function(data,
                                y_var = "percentage_median", 
                                x_var = "predictor",
                                fill_var = "sel_classification",
                                col_vec = palette_ecozones,
                                x_name = x_label) {
  circular_p <- 
    data %>%
    ggplot() + 
    #add lines for every 10 percent
    geom_hline(
      aes(yintercept = y), 
      data.frame(y = seq(0, 50, by = 10)),
      color = "grey"
    ) +
    geom_vline(
      xintercept = (0:3)+0.5,
      linetype = "dashed",
      linewidth = 0.1,
      color = "grey50"
    ) +
    geom_col(
      data = data  %>% 
        dplyr::filter(grepl("Unique_percent", variance_partition)),
      aes(x = get(x_var),
          y = get(y_var),
          fill = get(fill_var)),
      width = 0.6,
      position = position_dodge(width = 0.8), 
      alpha = 1) +
    geom_col(
      data = data %>% 
        dplyr::filter(grepl("Average.share_percent", variance_partition)), 
      aes(x = get(x_var),
          y = get(y_var),
          fill = get(fill_var)),
      width = .6,
      position = position_dodge(width = 0.8), 
      alpha = 0.4
    ) +
    scale_fill_manual(
      "",
      values = col_vec,
      drop = FALSE
    ) +
    scale_y_continuous(
      limits = c(-5, 50),
      expand = c(0, 0),
      breaks = c(0, 10,20, 30, 40, 50)
    ) +
    annotate(
      "text",
      x = c(rep(1.5,3), rep(2.5,3), rep(3.5,3)),
      y = rep(seq(20,40, by = 10),3),
      label = rep(paste0(seq(20,40, by = 10), " %"),3), 
      vjust = 0,
      size = 2) +
    coord_polar() +
    scale_x_discrete(
      label = x_name,
      drop = FALSE
    ) +
    labs( 
      title = "",
      x = "", 
      y = ""
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7),
      legend.key.size = unit(0.2, "cm"),
      panel.grid = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.ticks = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(colour = "grey30", size = 9, family = "sans", vjust = -3),
      text = element_text(color = "grey30"),
      plot.margin = unit(c(0.3, 0, 0, 0), "cm")
    ) 
  
  
  
  return(circular_p)
  
}

  