#' @title Add canonical region factor
#' @description Recode region names and cast to canonical region level order.
#' @param data_source Data frame with column `region`.
#' @return Data frame with factor column `region`.
add_region_as_factor <- function(data_source) {
  assertthat::assert_that(
    is.data.frame(data_source),
    msg = "`data_source` must be a data frame."
  )

  assertthat::assert_that(
    "region" %in% names(data_source),
    msg = "`data_source` must contain `region`."
  )

  res_data <-
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
    )

  return(res_data)
}
