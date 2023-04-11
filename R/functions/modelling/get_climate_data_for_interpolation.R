

get_climate_data_for_interpolation <- function(data_source,
                                               sel_var = c("temp_annual", 
                                                           "temp_cold", 
                                                           "prec_annual", 
                                                           "prec_summer", 
                                                           "prec_win")) {
  data_source %>%
    unnest(climate_data) %>%
    pivot_longer(temp_annual:gdm, 
                 names_to = "var_name", 
                 values_to = "value") %>%
    dplyr::select(-time_id) %>%
    nest(data_to_fit = c(-var_name)) %>%
    filter(var_name %in% all_of(sel_var)) %>%
    return()
}



