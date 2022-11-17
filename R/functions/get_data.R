#' @title Get pollen data from data assembly
#' @description Use data assembly derived from RFossilPol
#' @return Data assembly 

get_data <- function(file) {
  
  readr::read_rds(file) %>% 
    pluck("data") 
      
}