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
get_density_pap_combined <- function(data_source_change_points,
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
            age = ceiling(var)
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
    data_source_change_points %>%
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
      turnover_cp = purrr::pmap(
        .l = list(
          mvrt_cp,
          roc_cp,
          roc_pp,
          dcca_cp),
        .f = ~c(..1,..2,..3,..4)
        )
    ) %>%
    dplyr::mutate(
      density_turnover = purrr::pmap(
        .l = list(
          turnover_cp,
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
      density_diversity = purrr::pmap(
        .l = list(
          diversity_cp,
          dummy_table,
          age_min,
          age_max
        ),
        .f = ~ get_density_subset(
          data_source = ..1$age,
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
          density_turnover, # ..1
          density_diversity, # ..2
          dummy_table # ..3
        ),
        .f = ~ ..3 %>%
          dplyr::full_join(
            ..1 %>%
              dplyr::select(
                age,
                density_turnover = density
              ),
            by = "age"
          ) %>%
          dplyr::full_join(
            ..2 %>%
              dplyr::select(
                age,
                density_diversity = density
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
