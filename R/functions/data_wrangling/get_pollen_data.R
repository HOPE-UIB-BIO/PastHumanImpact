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
  assertthat::assert_that(
    is.data.frame(data_assembly),
    msg = "`data_assembly` must be a data frame."
  )

  assertthat::assert_that(
    is.character(variables) && length(variables) > 0,
    msg = "`variables` must be a non-empty character vector."
  )

  assertthat::assert_that(
    all(variables %in% names(data_assembly)),
    msg = "All requested `variables` must exist in `data_assembly`."
  )

  res_data <-
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
    )

  return(res_data)
}
