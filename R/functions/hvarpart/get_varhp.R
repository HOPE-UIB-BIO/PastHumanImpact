#' @title Hierarchial variation partitioning
#' @description Function to select response and predictor variables and run
#' hierarchical variation partitioning and permutation.
#' There is an oprion to use already calculated distance matrix by selecting
#' `reponse_vars` == NULL, and `data_response_dist` == [name of a column]
#' @param data_source full dataset with response and predictor variables
#' @param reponse_vars vector of names of response variables
#' @param data_response_dist
#' A name of column with already pre-calcuated distance
#' @param response_dist
#' NULL or a name of distance to be calculated with the function vegan::vegdist
#' @param predictor_vars
#' vector of names of predictor variables or list with names
#' @param run_all_predictors logical; if predictor variables should be assessed
#' individually or as list of data.frames
#' @param time_series logical; Should permutation be used for ordered?
#' @param get_significance logical; Should significance of predictors be
#' estimated? (takes along time)
#' @param permutations integers; numbers of permutations
#' @param ... see parameters of functions within
#' @return List of model outputs and a summary table of the results

get_varhp <- function(data_source,
                      response_dist = NULL,
                      data_response_dist = NULL,
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
                      predictor_vars = list(
                        human = c("spd"),
                        climate = c(
                          "temp_annual",
                          "temp_cold",
                          "prec_summer",
                          "prec_win"
                        ),
                        time = c("age")
                      ),
                      run_all_predictors = FALSE,
                      time_series = TRUE,
                      get_significance = TRUE,
                      permutations = 99,
                      ...) {
  tryCatch(
    {
      if (is.null(response_dist) & is.null(data_response_dist)) {
        # prepare responses without transformation
        data_resp <-
          data_source %>%
          dplyr::select(
            dplyr::all_of(response_vars)
          ) %>%
          dplyr::select(
            tidyselect:::where(~ any(!is.na(.)))
          )
      } else if (!is.null(response_dist)) {
        # prepare responses with distances
        data_resp <-
          data_source %>%
          dplyr::select(
            dplyr::all_of(response_vars)
          ) %>%
          dplyr::select(
            tidyselect:::where(~ any(!is.na(.)))
          )

        data_resp <- vegan::vegdist(data_resp, method = response_dist)
      } else if (!is.null(data_response_dist)) {
        # if input is already a distance matrix
        data_resp <- data_response_dist
        data_resp <- as.dist(data_resp)
      } else {
        stop("Something went wrong with response variables")
      }

      # prepare predictors
      # if `run_all_predictors` is true then use all variables individually
      if (
        isTRUE(run_all_predictors)
      ) {
        predictor_vars <-
          unlist(predictor_vars) %>%
          rlang::set_names(nm = NULL)

        data_preds <-
          data_source %>%
          dplyr::select(all_of(predictor_vars)) %>%
          janitor::remove_empty("cols") %>%
          janitor::remove_constant()

        output_table_dummy <-
          tibble::tibble(
            predictor = predictor_vars
          )
      } else {
        data_preds <-
          predictor_vars %>%
          purrr::map(
            .x = predictor_vars,
            .f = ~ data_source %>%
              dplyr::select(any_of(.x)) %>%
              janitor::remove_empty("cols") %>%
              janitor::remove_constant() %>%
              dplyr::select(
                tidyselect:::where(~ any(. != 0))
              )
          )

        # filer out groups with no variables
        data_preds <-
          data_preds[purrr::map_lgl(
            data_preds,
            .f = ~ ncol(.x) > 0
          )]

        output_table_dummy <-
          tibble::tibble(
            predictor = names(predictor_vars)
          )
      }


      # run hvarpar
      # should work for both list and just data.frame
      varhp <-
        rdacca_hp(
          dv = data_resp,
          iv = data_preds,
          method = "RDA",
          scale = TRUE,
          type = "adjR2",
          var.part = TRUE,
          ...
        )

      # extract relevant summary output
      output_table <-
        varhp %>%
        purrr::pluck("Hier.part") %>%
        as.data.frame() %>%
        tibble::rownames_to_column("predictor")

      # test significance
      if (
        isTRUE(get_significance)
      ) {
        # should work for both list and just data.frame
        hp_signif <-
          perm_hvarpart(
            dv = data_resp,
            iv = data_preds,
            method = "RDA",
            scale = TRUE,
            type = "adjR2",
            permutations = permutations,
            series = time_series,
            verbose = TRUE,
            ...
          )

        # extract relevant summary output
        output_table <-
          output_table %>%
          dplyr::left_join(
            hp_signif,
            by = "Individual"
          )
      }


      # left join with all predictors (from `output_table_dummy`)
      summary_table <-
        output_table_dummy %>%
        dplyr::left_join(
          output_table,
          by = "predictor"
        ) %>%
        # replace all missing values with 0
        dplyr::mutate(
          dplyr::across(
            tidyselect:::where(
              is.numeric
            ),
            ~ tidyr::replace_na(.x, replace = 0)
          )
        )

      results <-
        list(
          varhp_output = varhp,
          summary_table = summary_table
        )

      return(results)
    },
    error = function(err) NA
  )
}
