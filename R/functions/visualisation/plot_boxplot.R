plot_boxplot <- function(datasource,
                         y_var_name = "adj_r2_total",
                         groupings = "ecozone_koppen_5",
                         facet_name = "region",
                         label_x = "Ecozones",
                         label_y = "Total adjusted R2") {
  
 y_var_name <- as.character(y_var_name)
 groupings <- as.character(groupings)
 
 baseplot <- datasource %>%
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        y = get(y_var_name), 
        x = get(groupings), 
        fill = get(groupings)
        )
      ) +
    ggplot2::geom_boxplot() +
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
     )
 } else {
   finalplot <-
     baseplot
 }
 
}


