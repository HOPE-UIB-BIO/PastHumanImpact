#' @title Run hierarchial variation partitioning
#' @description This wrapper run the hierarchical variation
#' partitioning for the HOPE dataset.
#' There is an option to use already calculated distance matrix by selecting
#' `reponse_vars` == NULL, and `data_response_dist` == [name of a column]
#' @param data_source input tibble
#' @param reponse_vars
#' vector of variables to be included in the response datasetet
#' @param data_response_dist
#' A name of column with already pre-calcuated distance
#' @param response_dist
#' NULL or a name of distance to be calculated with the function vegan::vegdist
#' @param predictor_vars
#' vector of names of predictor variables or list with names
#' @param run_all_predictors logical; if predictor variables should be assessed
#' individual or as list of data frames
#' @param time_series logical; Should permutation be used for ordered?
#' @param get_significance logical; Should significance of predictors be
#' estimated? (takes along time)
#' @param permutations integer; number of permutations for p-values
#' @return
#' A data frame with original columns and a `varhp` list-column containing
#' outputs from `get_varhp()`.

run_hvarpart <- function(data_source,
                         response_vars = c(
                           "n0",
                           "n1",
                           "n2",
                           "n1_minus_n2",
                           "n2_divided_by_n1",
                           "n1_divided_by_n0",
                           "roc",
                           "dcca_axis_1",
                           "density_diversity",
                           "density_turnover"
                         ),
                         response_dist = NULL,
                         data_response_dist = NULL,
                         predictor_vars = list(
                           human = c("spd"),
                           climate = c(
                             "temp_annual",
                             "temp_cold",
                             "prec_summer",
                             "prec_win"
                           )
                         ),
                         run_all_predictors = FALSE,
                         time_series = TRUE,
                         get_significance = TRUE,
                         permutations = 99,
                         ...) {
  assertthat::assert_that(
    is.data.frame(data_source),
    msg = "`data_source` must be a data frame."
  )
  assertthat::assert_that(
    "data_merge" %in% names(data_source),
    msg = "`data_source` must contain a data_merge column."
  )

  if (!is.null(response_vars)) {
    assertthat::assert_that(
      is.character(response_vars),
      length(response_vars) > 0,
      msg = "`response_vars` must be a non-empty character vector or NULL."
    )
  }

  if (!is.null(data_response_dist)) {
    assertthat::assert_that(
      is.character(data_response_dist),
      length(data_response_dist) == 1,
      msg = "`data_response_dist` must be a single column name or NULL."
    )
    assertthat::assert_that(
      data_response_dist %in% names(data_source),
      msg = "`data_response_dist` must exist in `data_source`."
    )
  }

  res <- NULL

  if (!is.null(response_vars) & is.null(data_response_dist)
  ) {
    res <-
      data_source %>%
      dplyr::mutate(
        varhp = purrr::map(
          .x = data_merge,
          .f = ~ get_varhp(
            data_source = .x,
            permutations = permutations,
            response_dist = response_dist,
            response_vars = response_vars,
            predictor_vars = predictor_vars,
            run_all_predictors = run_all_predictors,
            time_series = time_series,
            get_significance = get_significance
          )
        )
      )
  } else if (!is.null(data_response_dist)) {
    res <-
      data_source %>%
      dplyr::mutate(
        data_response_dist = .data[[data_response_dist]]
      ) %>%
      dplyr::mutate(
        varhp = purrr::map2(
          .x = data_merge,
          .y = data_response_dist,
          .f = ~ get_varhp(
            data_source = .x,
            data_response_dist = .y,
            permutations = permutations,
            response_vars = NULL,
            response_dist = NULL,
            predictor_vars = predictor_vars,
            run_all_predictors = run_all_predictors,
            time_series = time_series,
            get_significance = get_significance
          )
        )
      )
  } else {
    cli::cli_abort("No response variables or distance matrix provided")
  }

  return(res)
}
