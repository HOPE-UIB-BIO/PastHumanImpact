# circular barplot

get_circular_barchart <- function(data,
                                  fill_eco = palette_eco,
                                  x_name = x_label){
  
p <- data %>%
    ggplot() + 
    #add lines for every 10 percent
    geom_hline(
      aes(yintercept = y), 
      data.frame(y = seq(0, 50, by = 10)),
      color = "lightgrey"
    ) +
    geom_col(data = data %>% 
               dplyr::filter(variance_partition == "Unique_percent_median"), 
             aes(x = predictor,
                 y = percentage_median,
                 fill = ecozone_koppen_5),
             position_dodge(width = 0.9), alpha = 1) +
    geom_col(data = data %>% 
               dplyr::filter(variance_partition == "Average.share_percent_median"),
             aes(x = predictor,
                 y = percentage_median,
                 fill = ecozone_koppen_5),
             position = position_dodge(width = 0.9), alpha = 0.4) +
    scale_fill_manual(values = fill_eco, drop = FALSE) +
    geom_segment(
      aes(x = predictor,
          y = 0,
          xend = predictor,
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
    scale_x_discrete(label = x_name, drop = FALSE) +
    coord_polar() +
    theme_minimal()+
    theme(
      legend.position = "none",
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
    labs( 
      # title = paste0(select_region),
      x = "", 
      y = ""
    )
  p

  }
  
  