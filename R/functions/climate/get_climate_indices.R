#' @title Prepare and/or calculate climate indices
#' @description This function source the climate data, restructure and rename
#' CHELSA variables, and estimate number of growing degree months per
#' year (gdm = months > 0 degree C)
#' @return A nested tibble with climate variables per dataset_id

get_climate_indices <- function(data_source, time_ref) {
  data_source %>%
    # get list of climate data for each dataset
    purrr::pluck("data", "climate") %>%
    # name the list with dataset_id
    rlang::set_names(
      nm = data_source %>%
        purrr::pluck("data", "dataset_id")
    ) %>%
    # loop trough and make a new dataset at the end
    purrr::map_dfr(
      .id = "dataset_id",
      .f = ~ .x %>%
        #  rename(time_id = time.id) %>%
        tidyr::pivot_wider(
          names_from = "variable",
          values_from = "value"
        ) %>%
        tidyr::pivot_longer(
          cols = tasmin1:tasmin9,
          names_to = "variable",
          values_to = "value"
        ) %>%
        dplyr::mutate(
          # transform to Celsius
          temp_c = value / 10 - 273.15,
          # test if temperature higher than 0 C degrees
          above_0 = temp_c > 0
        ) %>%
        dplyr::group_by(time_id) %>%
        dplyr::mutate(gdm = sum(above_0)) %>%
        dplyr::ungroup() %>%
        transform_ages(., time_ref) %>%
        dplyr::select(time_id, age, bio1:bio6, gdm) %>%
       # dplyr::mutate(bio1 = bio1*10,
       #               bio6 = bio6*10) %>%
        dplyr::distinct() %>%
        dplyr::rename(
          temp_annual = bio1,
          temp_cold = bio6,
          prec_annual = bio12,
          prec_summer = bio18,
          prec_win = bio19,
          prec_seasonality = bio15
        ) %>%
        return()
    ) %>%
    tidyr::nest(climate_data = -dataset_id)  %>% 
    return()
}
