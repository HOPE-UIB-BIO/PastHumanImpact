#' @title Get density variables for modelling
#' @description
#' This function creates two combined density variables to be used
#' for hierarchical gam modeling
#' @param data_source_density
#' the individual density estimates of pap change points
#' @param data_rescaled
#' Logical. Should the density estimate rescaled to 0-1 be used?
#' @param select_vars
#' Character vector. Select the variables in varname to be combined
#' @return Provide common trend of the two combined density variables of
#' change points in diversity and composition
#'

get_density_for_modelling <- function(data_source_density,
                                      data_rescaled = TRUE,
                                      select_vars = NULL) {
  data_sub <-
    data_source_density %>%
    dplyr::select(dataset_id, pap_density_rescale)
  if (
    isTRUE(data_rescale)
  ) {
    data_unnest <-
      data_sub %>%
      tidyr::unnest(pap_density_rescale)
  } else {
    data_unnest <-
      data_sub %>%
      tidyr::unnest(pap_density)
  }
  data_unnest %>%
    tidyr::pivot_longer(mvrt:n2_divided_by_n1,
      names_to = "var_name",
      values_to = "value"
    ) %>%
    dplyr::filter(var_name %in% select_vars) %>%
    tidyr::drop_na(value) %>%
    tidyr::nest(data_to_fit = c(dataset_id, age, value)) %>%
    return()
}
