#' @title A wrapper function to run Principal Canonical Analysis
#' @param x Input is the response dataset as a data frame, matrix, tibble

run_pca <- function(x, scale = TRUE){
  resp <- x %>% 
    dplyr::select(n0:density_diversity)
  mod <- vegan::rda(resp, 
                    scale = scale, 
                    data = x)
  return(mod)
}