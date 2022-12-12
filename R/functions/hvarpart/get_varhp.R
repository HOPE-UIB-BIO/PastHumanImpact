#' @title Hierarchial variation partitioning
#' @description Function to select response and predictor variables and run
#' hierarchical variation partitioning and permutation
#' @param data_source full dataset with response and predictor variables
#' @param reponse_vars vector of names of response variables
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
                      reponse_vars = c(
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
                      time_series = TRUE,
                      get_significance = TRUE,
                      permutations = 99,
                      ...) {
  # prepare responses
  data_resp <-
    data_source %>%
    dplyr::select(all_of(reponse_vars))

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
      dplyr::select(dplyr::all_of(predictor_vars)) %>%
      # note have to remove empty vars for individual sites or the model
      #  will fail, need to take this into account later (for discussion
      #   - empty vars = different things)
      dplyr::select(tidyselect:::where(~ any(. != 0)))

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
          dplyr::select(dplyr::all_of(.x)) %>%
          # note have to remove empty vars for individual sites or the model
          #  will fail, need to take this into account later (for discussion
          #   - empty vars = different things)
          dplyr::select(tidyselect:::where(~ any(. != 0)))
      )

    output_table_dummy <-
      tibble::tibble(
        predictor = names(predictor_vars)
      )
  }

  # run hvarpar
  # should work for both list and just data.frame
  varhp <-
    rdacca.hp::rdacca.hp(
      dv = vegan::vegdist(data_resp, method = "gower"),
      iv = data_preds,
      add = TRUE,
      method = "dbRDA",
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
        dv = vegan::vegdist(data_resp, method = "gower"),
        iv = data_preds,
        method = "dbRDA",
        add = TRUE,
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

  # run model to get variation inflation factors of the predictors,
  #   and total unexplained and explained variation
  mod <-
    vegan::capscale(data_resp ~ as.matrix(as.data.frame(data_preds)),
      dist = "gower",
      add = TRUE,
      data_source = data_resp
    )

  data_variation <-
    tibble::tibble(
      Total_eig = mod %>%
        purrr::pluck("tot.chi"),
      Constrained_eig = mod %>%
        purrr::pluck("CCA") %>%
        purrr::pluck("eig") %>%
        sum(),
      Unconstrained_eig = Total_eig - Constrained_eig
    )

  # add additional VIF information
  # only works for all predictors
  if (
    isTRUE(run_all_predictors)
  ) {
    output_table <-
      output_table %>%
      dplyr::mutate(
        Vif = vegan::vif.cca(mod),
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
      summary_table = summary_table,
      summary_variation = data_variation
    )

  return(results)
}
