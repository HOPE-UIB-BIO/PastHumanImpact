get_names_from_full_model_name <- function(data_source) {
  data_source %>%
    dplyr:::mutate(
      predictor = purrr::map_chr(
        .x = full_name,
        .f = ~ dplyr::case_when(
          stringr::str_detect(.x, "temp_annual") ~ "temp_annual",
          stringr::str_detect(.x, "temp_cold") ~ "temp_cold",
          stringr::str_detect(.x, "prec_annual") ~ "prec_annual",
          stringr::str_detect(.x, "prec_summer") ~ "prec_summer",
          stringr::str_detect(.x, "prec_win") ~ "prec_win",
          stringr::str_detect(.x, "spd") ~ "spd",
          TRUE ~ NA_character_
        )
      ),
      region_raw = purrr::map_chr(
        .x = full_name,
        .f = ~ dplyr::case_when(
          stringr::str_detect(.x, "Africa") ~ "Africa",
          stringr::str_detect(.x, "Asia") ~ "Asia",
          stringr::str_detect(.x, "Europe") ~ "Europe",
          stringr::str_detect(.x, "North.America") ~ "North.America",
          stringr::str_detect(.x, "Latin.America") ~ "Latin.America",
          stringr::str_detect(.x, "Oceania") ~ "Oceania",
          TRUE ~ NA_character_
        )
      ),
      region = stringr::str_replace(region_raw, "\\.", " "),
      group = stringr::str_replace(
        full_name,
        pattern = paste0(
          paste(predictor, region_raw, sep = "_"),
          "_"
        ),
        replacement = ""
      )
    ) %>%
    dplyr::select(-c( region_raw)) %>%
    return()
}
