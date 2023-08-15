# circular barplot

get_circular_barchart <- function(data,
                                  y_var = "percentage", 
                                  x_var = "predictor",
                                  fill_var = "ecozone_koppen_5",
                                  col_vec = palette_eco,
                                  x_name = x_label,
                                  title = "h1") {
  
p <- data %>%
    ggplot() + 
    #add lines for every 10 percent
    geom_hline(
      aes(yintercept = y), 
      data.frame(y = seq(0, 50, by = 10)),
      color = "lightgrey"
    ) +
    geom_col(data = data %>% 
               dplyr::filter(grepl("Unique_percent", variance_partition)), 
             aes(x = get(x_var),
                 y = get(y_var),
                 fill = get(fill_var)),
             position_dodge(width = 0.9), alpha = 1) +
    geom_col(data = data %>% 
               dplyr::filter(grepl("Average.share_percent", variance_partition)),
             aes(x = get(x_var),
                 y = get(y_var),
                 fill = get(fill_var)),
             position = position_dodge(width = 0.9), alpha = 0.4) +
    scale_fill_manual("",
                      values = col_vec, 
                      drop = FALSE) +
    geom_segment(
      aes(x = get(x_var),
          y = 0,
          xend = get(x_var),
          yend = 50
      ),
      linetype = "dashed",
      linewidth = 0.3,
      color = "grey50"
        
    ) +
    scale_y_continuous(
      limits = c(-10, 50),
      expand = c(0, 0),
      breaks = c(0, 10,20, 30, 40, 50)
    ) +
    annotate("text",
             x = rep(seq(1,3, by = 1),4),
             y = rep(seq(10,40, by = 10),3),
             label = rep(paste0(seq(10,40, by = 10), " %"),3), 
             vjust = 0,
             size = 3) +
    coord_polar() +
    theme_minimal()+
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7),
      legend.key.size = unit(0.2, "cm"),
      panel.grid = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.ticks = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(colour = "grey30", size = 10, family = "sans", vjust = -3),
      #axis.title.x = element_text(margin = margin(0, 0, -2, 0)),
      text = element_text(color = "grey30"),
      plot.title = element_text(family = "sans",size = 12, hjust = 0.5, margin = margin(0,0,0,0)),
      plot.margin = unit(c(0.3, 0, 0, 0), "cm")
    ) +
 
    scale_x_discrete(label = x_name, 
                     drop = FALSE) +
    labs( 
      title = title,
      x = "", 
      y = ""
    ) 
  p

  }
  
  