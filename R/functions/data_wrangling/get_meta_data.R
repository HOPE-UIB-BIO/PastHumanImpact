#' @title Get meta data and add age limits
#' @description Get a subset of the full data assembly derived from RFossilpol
#' and add age litims of data
#' @return
#' Get a reduced tibble of selected variables from the full data assembly

get_meta_data <- function(data_assembly,
                          variables = c(
                            "dataset_id",
                            "handle",
                            "country",
                            "long",
                            "lat",
                            "altitude",
                            "depositionalenvironment",
                            "region",
                            "curve_name",
                            "ecozone_koppen_5",
                            "ecozone_koppen_15",
                            "ecozone_koppen_30",
                            "data_publicity",
                            "doi"
                          )) {
  data_assembly %>%
    dplyr::mutate(
      age_lim = purrr::map(
        .x = levels,
        .f = ~ .x %>%
          purrr::pluck("age") %>%
          range()
      ),
      age_min = purrr::map_dbl(
        .x = age_lim,
        .f = ~ min(.x)
      ),
      age_max = purrr::map_dbl(
        .x = age_lim,
        .f = ~ max(.x)
      )
    ) %>%
    dplyr::select(
      dplyr::all_of(variables), age_min, age_max
    ) %>% 
    dplyr::mutate(
      climate_ecozone = dplyr::case_when(
        ecozone_koppen_15 == "Cold_Without_dry_season" ~ ecozone_koppen_30,
        ecozone_koppen_5 == "Cold" ~ ecozone_koppen_15,
        ecozone_koppen_5 == "Temperate" ~ ecozone_koppen_15,
        .default = ecozone_koppen_5
      )
    ) %>%
    return()
}
