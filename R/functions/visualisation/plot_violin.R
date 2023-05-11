plot_violin <- function(data_source,
                        y_var_name = "adj_r2_total",
                        groupings = "ecozone_koppen_5",
                        facet_name = "region",
                        label_x = "Ecozones",
                        label_y = "Total adjusted R2") {
  
  
  y_var_name <- as.character(y_var_name)
  groupings <- as.character(groupings)
  
  baseplot <- data_source %>%
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        y = get(y_var_name), 
        x = get(groupings), 
        fill = get(groupings)
      )
    ) +
    ggplot2::geom_violin() +
    ggplot2::geom_boxplot(width = 0.1, fill = "white") +
    ggplot2::scale_fill_hue(c = 50, l = 60, h = c(30, 300)) +
    ggplot2::labs(
      x = label_x, 
      y = label_y
    )
  
  if (
    isFALSE(is.null(facet_name))
  ) {
    facet_name <- as.character(facet_name)
    
    finalplot <-
      baseplot +
      ggplot2::facet_wrap(
        as.formula(
          paste(
            "~",
            paste(facet_name, collapse = " + ")
          )
        )
      ) + 
      ggplot2::theme(
        legend.position = "none"
      ) 
  } else {
    finalplot <-
      baseplot + 
      ggplot2::theme(
        legend.position = "none"
      ) 
  }
  return(finalplot)
  
}

