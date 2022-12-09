#' @title Run hierarchial variation partitioning
#' @description This wrapper run the hierarchical variation
#' partitioning for the HOPE dataset
#' @param data_source input tibble
#' @param permutations integer; number of permutations for p-values
#' @param reponse_vars
#' vector of variables to be included in the response datasetet
#' @param predictor_vars
#' vector of names of predictor variables or list with names
#' @param run_all_predictors logical; if predictor variables should be assessed
#' individual or as list of data frames
#' @param time_series logical; Should permutation be used for ordered?
#' @return List of model outputs and a summary table of the results


run_hvarpart <- function(data_source,
                         permutations = 99,
                         response_vars = c(
                           "n0", "n1", "n2",
                           "n1_minus_n2", "n2_divided_by_n1", "n1_divided_by_n0",
                           "roc",
                           "dcca_axis_1"
                         ),
                         predictor_vars = list(
                           human = c("spd"),
                           climate = c(
                             "temp_cold",
                             "prec_summer",
                             "prec_win",
                             "gdm"
                           ),
                           time = c("age")
                         ),
                         run_all_predictors = FALSE,
                         time_series = TRUE) {
  res <- data_source %>%
    mutate(varhp = purrr::map(
      data_merge,
      ~ get_varhp(.,
        permutations = permutations,
        response_vars = response_vars,
        predictor_vars = predictor_vars,
        run_all_predictors = run_all_predictors,
        time_series = time_series
      )
    ))

  return(res)
}
