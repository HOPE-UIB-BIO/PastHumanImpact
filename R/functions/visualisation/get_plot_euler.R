#' @title Plot Euler diagrams
#' @description Create Euler diagrams with ggplot
#' @param data_for_euler The data structure for input to plot Euler diagrams
#' @return Euler plot

get_plot_euler <- function(data_for_euler, alpha = .7) {
  
  v <- venneuler::venneuler(data_for_euler)
  
  newdf <- data.frame(v$centers, 
                      diameters = v$diameters, 
                      predictors = v$labels, 
                      stringsAsFactors = FALSE) %>%
    mutate(r = diameters/2)
  
  
  
  plt <- newdf %>%
    ggplot() +
    ggforce::geom_circle(aes(x0 = x, y0 = y, r = r, fill = predictors), colour = NA, alpha = alpha) +
    coord_fixed() +
    theme_void() +
    theme(legend.position = "none") +
    theme_transparent()
  
  plt
  
}  