plot_barplot <- function(data_source,
                         y_var_name = "individual",
                         #grouping =  "ecozone_koppen_5",
                         facet_name = "region",
                         x_label = "Predictor",
                         y_label = "Explained variation",
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

