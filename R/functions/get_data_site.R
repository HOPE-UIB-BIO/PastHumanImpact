#' @title Get site data from data assembly
#' @description Use data assembly derived from RFossilpol
#' @return For each site a tibble with dataset_id, geographical coordinates, altitude, depositional environment, country, region, and ecozone

get_data_site <- function(data_assembly) {
  
  data_assembly %>%
    dplyr::select(dataset_id, 
                  handle, lat, long,  
                  altitude, depositionalenvironment, 
                  region, ecozone_koppen_5)
}
