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
      color = "grey90"
    ) +
    geom_vline(
      xintercept = (0:3)+0.5,
      linetype = "dashed",
      linewidth = 0.01,
      color = "grey90"
    ) +
    geom_col(
      data = data  %>% 
        dplyr::filter(grepl("Unique_percent", 
                            variance_partition)),
      aes(x = get(x_var),
          y = get(y_var),
          fill = get(fill_var)),
      width = 0.6,
      position = position_dodge2(width = 0.8, 
                                 preserve = "single"), 
      alpha = 1) +
    geom_col(
      data = data %>% 
        dplyr::filter(grepl("Average.share_percent", 
                            variance_partition)), 
      aes(x = get(x_var),
          y = get(y_var),
          fill = get(fill_var)),
      width = .6,
      position = position_dodge2(width = 0.8, 
                                 preserve = "single"), 
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
      x = c(3.5, 3.5, 3.5),
      y = seq(20,40, by = 10),
      label = paste0(seq(20,40, by = 10), " %"), 
      vjust = 0,
      size = 1.5
      ) +
    coord_polar(
      
    ) +
    scale_x_discrete(
      label = x_name,
      drop = FALSE
    ) +
    labs( 
      title = "",
      x = "", 
      y = ""
    ) +
    theme(
      legend.position = "none",
      panel.background  = element_blank(),
      panel.border = element_blank(),
      plot.background = element_blank(), 
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = 6, 
                                 vjust = -3),
      plot.margin = grid::unit(c(0, 0, 0, 0), "mm"),
      panel.spacing=unit(c(0,0,0,0), "null")
      
    ) 
  
  
  
  return(circular_p)
  
}

  