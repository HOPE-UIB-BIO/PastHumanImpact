get_pcoa_model <- function(data, dist = "gower", add = TRUE) {
  
  mod <-  vegan::capscale(data %>% dplyr::select(n0:roc) ~ 1,
                          dist = dist,
                          add = add,
                          data = data) 

 
  return(mod)
}
