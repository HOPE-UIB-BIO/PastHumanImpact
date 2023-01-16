plot_barplot <- function(data_source,
                         y_var_name = "individual",
                         #grouping =  "ecozone_koppen_5",
                         facet_name = "region",
                         x_label = "Predictor",
                         y_label = "Explained variation",
                         theme_map = FALSE,
                         ...) {
  

  y_var_name <- as.character(y_var_name)
  #grouping <- as.character(grouping)
  
 baseplot <- data_source %>% 
   ggplot(mapping = ggplot2::aes(
     y = get(y_var_name), 
     x = reorder(as.factor(predictor), - get(y_var_name)), 
               fill = as.factor(predictor)), 
    ) +
    ggplot2::geom_bar(
      stat = "identity", 
      alpha = 0.5
      ) +
    ggplot2::labs(
      x = x_label, 
      y = y_label
      ) + 
   ggplot2::theme(
     legend.position = "none"
   ) 
 
 # add facet
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
     ) 
 } else {
   finalplot <-
     baseplot 
 }
 
 # plot theme for map
 if(
   isTRUE(theme_map)
 ) {
   finalplot <- 
     finalplot +
     ggplot2::theme(
       panel.background = element_rect(fill = "transparent"),
       plot.background = element_rect(fill = "transparent"),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
       axis.text.x = element_blank()
     ) 
 }
 
 return(finalplot)
 
}

