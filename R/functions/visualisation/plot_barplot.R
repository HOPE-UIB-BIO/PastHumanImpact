plot_barplot <- function(datasource,
                         y_var_name,
                         grouping = "predictor",
                         facet_var_name = NULL,
                         x_label = "Predictor",
                         y_label = "Mean adj r2",
                         ...) {
  
  
  y_var_name <- as.character(y_var_name)
  grouping <- as.character(grouping)
  
 baseplot <- datasource %>% 
   ggplot(mapping = ggplot2::aes(
     y = get(y_var_name), 
     x = reorder(as.factor(get(predictor)), - get(y_var_name)), 
               fill = as.factor(get(predictor))), 
     ...) +
    ggplot2::geom_bar(
      stat = "identity", 
      alpha = 0.5,
      ...) +
    ggplot2::labs(
      x = x_label, 
      y = y_label,
      ...)
 
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
 
}

