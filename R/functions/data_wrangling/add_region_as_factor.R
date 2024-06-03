add_region_as_factor <- function(data_source) {
  data_source %>%
    dplyr::mutate(
      region = dplyr::case_when(
        .default = region,
        region == "Latin America" ~ "Central & South America"
      ),
      region = factor(
        region,
        levels = c(
          "North America",
          "Central & South America",
          "Europe",
          "Asia",
          "Oceania"
        )
      )
    ) %>%
    return()
}
