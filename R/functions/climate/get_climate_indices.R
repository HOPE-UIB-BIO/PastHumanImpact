#' @title Prepare and/or calculate climate indices
#' @description This function source the climate data, restructure and rename CHELSA variables, and estimate number of growing degree months per year (gdm = months > 0 degree C)
#' @return A nested tibble with climate variables per dataset_id

get_climate_indices <- function(data_source, time_ref) {
  data_source %>%
    purrr::map(climate, function(.x){
      .x %>%
        #  rename(time_id = time.id) %>%
        pivot_wider(names_from = variable, values_from = value) %>%
        pivot_longer(tasmin1:tasmin9, names_to = "variable", values_to = "value") %>%
        mutate(temp_c = value/10 - 273.15, # transform to Celsius
               above_0 = temp_c > 0) %>% # test if temperature higher than 0 C degrees
        group_by(time_id) %>%
        mutate(gdm = sum(above_0)) %>% 
        ungroup() %>%
        transform_ages(. , time_ref) %>%
        dplyr::select(time_id, age, bio1:bio6, gdm) %>%
        distinct() %>%
        rename(temp_annual = bio1,
               temp_cold = bio6,
               prec_annual = bio12,
               prec_summer = bio18,
               prec_win = bio19,
               prec_seasonality = bio15)
    })
}


