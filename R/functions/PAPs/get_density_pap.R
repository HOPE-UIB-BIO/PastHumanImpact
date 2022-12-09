#' @title Get density of change points
#' @description
#' A function to get the temporal density of regression tree change points
#' of pollen assemblage properties
#' @param data_source the resulting data from the estimation of change points
#' @param data_source_meta input is the data_meta to extract site information
#' @param data_source_dummy_time
#' is a table to create the time variables on even space
#' output for the densities
#' @param limit_length Logical. Should the variables be limited by max and min ages?
#' @return Turn change points of pap variables into density variables
#'
get_density_pap <- function(data_soure_change_points,
                            data_source_meta,
                            data_source_dummy_time,
                            limit_length = TRUE) {
  # helper function
  get_density_subset <-
    function(data_source,
             dummy_table,
             age_min,
             age_max,
             ...) {
      if (
        isTRUE(is.null(data_source))
      ) {
        res <-
          dummy_table %>%
          dplyr::mutate(
            density = 0
          )
      } else {
        res <-
          REcopol::get_density(
            data_source = data_source,
            reflected = TRUE,
            values_range = c(
              age_min = age_min,
              age_max = age_max
            ),
            bw = 1000 / max(dummy_table$age),
            n = max(dummy_table$age),
            ...
          ) %>%
          dplyr::mutate(
            age = round(var)
          ) %>%
          dplyr::filter(
            age %in% dummy_table$age
          ) %>%
          dplyr::select(
            age, density
          )
      }
      return(res)
    }

  data_age_lim <-
    data_source_meta %>%
    dplyr::select(
      dataset_id, age_min, age_max
    ) %>%
    # add dummy table
    dplyr::mutate(
      dummy_table = list(data_source_dummy_time)
    )

  data_source_main <-
    data_soure_change_points %>%
    dplyr::left_join(
      data_age_lim,
      by = "dataset_id"
    )

  if (
    isTRUE(limit_length)
  ) {
    data_source_main <-
      data_source_main %>%
      dplyr::mutate(
        dummy_table = purrr::pmap(
          .l = list(dummy_table, age_min, age_max),
          .f = ~ ..1 %>%
            dplyr::filter(
              age >= ..2 & age <= ..3
            )
        )
      )
  }

  data_cp_density <-
    data_source_main %>%
    dplyr::mutate(
      mvrt_cp_density = purrr::pmap(
        .l = list(
          mvrt_cp,
          dummy_table,
          age_min,
          age_max
        ),
        .f = ~ get_density_subset(
          data_source = ..1,
          dummy_table = ..2,
          age_min = ..3,
          age_max = ..4
        )
      ),
      roc_cp_density = purrr::pmap(
        .l = list(
          roc_cp,
          dummy_table,
          age_min,
          age_max
        ),
        .f = ~ get_density_subset(
          data_source = ..1,
          dummy_table = ..2,
          age_min = ..3,
          age_max = ..4
        )
      ),
      roc_pp_density = purrr::pmap(
        .l = list(
          roc_pp,
          dummy_table,
          age_min,
          age_max
        ),
        .f = ~ get_density_subset(
          data_source = ..1,
          dummy_table = ..2,
          age_min = ..3,
          age_max = ..4
        )
      ),
      dcca_cp_density = purrr::pmap(
        .l = list(
          dcca_cp,
          dummy_table,
          age_min,
          age_max
        ),
        .f = ~ get_density_subset(
          data_source = ..1,
          dummy_table = ..2,
          age_min = ..3,
          age_max = ..4
        )
      ),
      n0_density = purrr::pmap(
        .l = list(
          diversity_cp,
          dummy_table,
          age_min,
          age_max
        ),
        .f = ~ ..1 %>%
          dplyr::filter(var_name == "n0") %>%
          purrr::pluck("age") %>%
          get_density_subset(
            data_source = .,
            dummy_table = ..2,
            age_min = ..3,
            age_max = ..4
          )
      ),
      n1_density = purrr::pmap(
        .l = list(
          diversity_cp,
          dummy_table,
          age_min,
          age_max
        ),
        .f = ~ ..1 %>%
          dplyr::filter(var_name == "n1") %>%
          purrr::pluck("age") %>%
          get_density_subset(
            data_source = .,
            dummy_table = ..2,
            age_min = ..3,
            age_max = ..4
          )
      ),
      n2_density = purrr::pmap(
        .l = list(
          diversity_cp,
          dummy_table,
          age_min,
          age_max
        ),
        .f = ~ ..1 %>%
          dplyr::filter(var_name == "n2") %>%
          purrr::pluck("age") %>%
          get_density_subset(
            data_source = .,
            dummy_table = ..2,
            age_min = ..3,
            age_max = ..4
          )
      ),
      n1_divided_by_n0_density = purrr::pmap(
        .l = list(
          diversity_cp,
          dummy_table,
          age_min,
          age_max
        ),
        .f = ~ ..1 %>%
          dplyr::filter(var_name == "n1_divided_by_n0") %>%
          purrr::pluck("age") %>%
          get_density_subset(
            data_source = .,
            dummy_table = ..2,
            age_min = ..3,
            age_max = ..4
          )
      ),
      n2_divided_by_n1_density = purrr::pmap(
        .l = list(
          diversity_cp,
          dummy_table,
          age_min,
          age_max
        ),
        .f = ~ ..1 %>%
          dplyr::filter(var_name == "n2_divided_by_n1") %>%
          purrr::pluck("age") %>%
          get_density_subset(
            data_source = .,
            dummy_table = ..2,
            age_min = ..3,
            age_max = ..4
          )
      )
    )

  data_cp_density_merge <-
    data_cp_density %>%
    dplyr::mutate(
      pap_density = purrr::pmap(
        .l = list(
          mvrt_cp_density, # ..1
          roc_cp_density, # ..2
          roc_pp_density, # ..3
          dcca_cp_density, # ..4
          n0_density, # ..5
          n1_density, # ..6
          n2_density, # ..7
          n1_divided_by_n0_density, # ..8
          n2_divided_by_n1_density, # ..9
          dummy_table # ..10
        ),
        .f = ~ ..10 %>%
          dplyr::full_join(
            ..1 %>%
              dplyr::select(
                age,
                mvrt = density
              ),
            by = "age"
          ) %>%
          dplyr::full_join(
            ..2 %>%
              dplyr::select(
                age,
                roc = density
              ),
            by = "age"
          ) %>%
          dplyr::full_join(
            ..4 %>%
              dplyr::select(
                age,
                peakpoints = density
              ),
            by = "age"
          ) %>%
          dplyr::full_join(
            ..4 %>%
              dplyr::select(
                age,
                dcca = density
              ),
            by = "age"
          ) %>%
          dplyr::full_join(
            ..5 %>%
              dplyr::select(
                age,
                n0 = density
              ),
            by = "age"
          ) %>%
          dplyr::full_join(
            ..6 %>%
              dplyr::select(
                age,
                n1 = density
              ),
            by = "age"
          ) %>%
          dplyr::full_join(
            ..7 %>%
              dplyr::select(
                age,
                n2 = density
              ),
            by = "age"
          ) %>%
          dplyr::full_join(
            ..8 %>%
              dplyr::select(
                age,
                n1_divided_by_n0 = density
              ),
            by = "age"
          ) %>%
          dplyr::full_join(
            ..9 %>%
              dplyr::select(
                age,
                n2_divided_by_n1 = density
              ),
            by = "age"
          )
      )
    )

  # subset data and rescale to 1
  data_cp_density_merge %>%
    dplyr::select(dataset_id, pap_density) %>%
    dplyr::mutate(
      # rescale to 1
      pap_density_rescale = purrr::map(
        .x = pap_density,
        .f = ~ .x %>%
          dplyr::mutate(
            dplyr::across(
              .cols = -age,
              .fns = ~ scales::rescale(.x, to = c(0, 1))
            )
          ) %>%
          return()
      )
    )  %>% 
    return()
}
