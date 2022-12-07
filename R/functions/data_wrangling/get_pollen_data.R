#' @title Get pollen data and and percentages
#' @description Get a subset of the full data assembly derived from RFossilpol
#' and  add percentages
#' @return
#' Get a reduced tibble of selected variables from the full data assembly

get_pollen_data <- function(data_assembly,
                            variables = c(
                              "dataset_id",
                              "counts_harmonised",
                              "levels",
                              "age_uncertainty",
                              "pollen_percentage",
                              "end_of_interest_period"
                            )) {
  data_assembly %>%
    dplyr::select(
      dplyr::all_of(variables)
    ) %>%
    dplyr::mutate(
      percentages_harmonised = purrr::map(
        .x = counts_harmonised,
        .f = ~ REcopol:::transfer_into_proportions(
          data_source = .x,
          method = "percentages"
        )
      )
    ) %>%
    return()
}
