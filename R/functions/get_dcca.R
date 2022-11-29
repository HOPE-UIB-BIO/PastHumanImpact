#' @title Get estimates of pollen compositional turnover with time using
#' Detrended Canonical Correspondence Analysis (DCCA)
#' @description Use pollen assemblage data and age/time as constraint
#' @param data_pollen Use pollen percentages
#' @param sel_method Choose unconstrained or constrained ordination method.
#' The constrained version is DCCA with Canoco 4.5, and it can only be run
#' on the Windows platform, the unconstrained version is run using
#' vegan::decorana() in R and can be run on all platforms
#' @param var_name_pred Name of the constraining variable
#' @param sel_complexity Allow flexibility using second or third order
#' polynomials of the constraining variable
#' @param transform_to_percentage TRUE or FALSE depending on input data,
#' provide configuration for Canoco 4.5
#' @param transformation can be none, square-root or log
#' @return For each dataset the DCCA results, the weighted average scores
#' for each level for DCCA axis 1 (case_r scores), and total gradient length
#' for DCCA 1

get_dcca <- function(data_pollen, ...) {
  data_dcca <-
    data_pollen %>%
    dplyr::mutate(
      dcca = purrr::map2(
        .x = percentages_harmonised,
        .y = levels,
        .f = ~ REcopol::fit_ordination(
          data_source_community = .x,
          data_source_predictors = .y,
          sel_method = "constrained",
          var_name_pred = "age",
          sel_complexity = "poly_2",
          transform_to_percentage = FALSE,
          tranformation = "none"
        )
      )
    )

  data_dcca_proc <-
    data_dcca %>%
    dplyr::mutate(
      dcca_scores = purrr::map(
        .x = dcca,
        .f = ~ .x %>% purrr::pluck("case_r")
      ),
      dcca_grad_length = purrr::map_dbl(
        .x = dcca,
        .f = ~ .x %>% purrr::pluck("axis_1_grad_length")
      )
    )

  data_turnover <-
    data_dcca_proc %>%
    dplyr::select(
      dataset_id,
      dcca,
      dcca_scores,
      dcca_grad_length
    )

  return(data_turnover)
}
