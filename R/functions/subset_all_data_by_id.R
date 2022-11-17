#' @title Subset data assemblage by valid `sample_id`
#' @param data_source Data assembly. Table containing variables defined
#' by `variable_vec`and `valid_id`.
#' @param variable_vec Vector with names of columns which should be filtered
#' @return Data assembly with all level-related columns filtered by `valid_id`
subset_all_data_by_id <-
  function(data_source,
           variable_vec = c(
             "levels",
             "raw_counts",
             "counts_harmonised",
             "age_uncertainty"
           )) {
    check_class("data_source", "data.frame")
    
    check_col_names(
      "data_source",
      c(
        variable_vec,
        "valid_id"
      )
    )
    
    res <- data_source
    
    for (i in seq_along(variable_vec)) {
      sel_var <- variable_vec[i]
      
      if (
        sel_var == "age_uncertainty"
      ) {
        res <-
          res %>%
          dplyr::mutate(
            age_uncertainty = purrr::map2(
              .x = age_uncertainty,
              .y = valid_id,
              .f = ~ subset_uncertainty_matrix(
                data_source = .x,
                level_vector = .y
              )
            )
          )
        
        check_col_names("res", "age_uncertainty")
      } else {
        res <-
          res %>%
          dplyr::mutate(
            !!sel_var := purrr::map2(
              .x = get(sel_var),
              .y = valid_id,
              .f = ~ subset_levels(
                data_source = .x,
                level_vector = .y
              )
            )
          )
        
        check_col_names("res", eval(sel_var))
      }
    }
    
    res %>%
      dplyr::select(-valid_id) %>%
      return()
  }