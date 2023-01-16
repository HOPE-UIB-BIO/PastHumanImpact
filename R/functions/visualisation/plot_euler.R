#' @title Plot Euler diagrams
#' @description Create Euler diagrams with ggplot
#' @param data_euler Data input restructured to plot Euler diagrams
#' @return Euler diagrams

plot_euler <- function(data_source, 
                       alpha = 0.7,
                       legend.position = "none",
                       ...) {
  
  
  v <- venneuler::venneuler(data_source)
  
   euler_df <- data.frame(v$centers, 
                      diameters = v$diameters, 
                      predictors = v$labels, 
                      stringsAsFactors = FALSE) %>%
    mutate(r = diameters/2)
  
  
  
  eulerdiagram <- euler_df %>%
    ggplot() +
    ggforce::geom_circle(
      mapping = ggplot2::aes(
        x0 = x, 
        y0 = y, 
        r = r, 
        fill = predictors), 
      colour = NA, 
      alpha = alpha,
      ...) +
    ggplot2::coord_fixed(
      ...
      ) +
    ggplot2::theme_void(
      ...
    ) +
    ggplot2::theme(
      legend.position = legend.position,
      ...
    ) +
    ggpubr::theme_transparent(
    ...
    )
  
  eulerdiagram 
  
}  
