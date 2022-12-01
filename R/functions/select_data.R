#' @title Select data and variables from data assembly
#' @description Get a subset of the full data assembly derived from RFossilpol
#' and potentially add percentages
#' @return
#' Get a reduced tibble of selected variables from the full data assembly

select_data <- function(data_assembly,
                        variables = NULL,
                        add_percentages = TRUE) {
  if (add_percentages == TRUE) {
    data_assembly %>%
      dplyr::mutate(
        percentages_harmonised =
          purrr::map(counts_harmonised,
            .f = ~ REcopol:::transfer_into_proportions(
              data_source = .x,
              method = "percentages"
            )
          )
      ) %>%
      dplyr::select(dplyr::all_of(variables), percentages_harmonised)
  } else {
    data_assembly %>%
      dplyr::select(dplyr::all_of(variables))
  }
}
