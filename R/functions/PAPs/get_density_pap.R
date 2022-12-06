#' @title Get density of change points 
#' @description A function to get the temporal density of regression tree change points of pollen assemblage properties
#' @return A new dataset with density variables
#' 
get_density_pap <- function(data_source,
                            age_table,
                            ) {
  
  
  # helper function
  get_density_subset <-
    function(data_source,
             age_table) {
      if (
        is.null(data_source)
      ) {
        res <-
          age_table %>%
          dplyr::mutate(
            density = 0
          )
      } else {
        res <-
          REcopol::get_density(
            data_source = data_source,
            reflected = TRUE,
            values_range = c(
              min(age_table$age),
              max(age_table$age)
            ),
            bw = 1000 / max(age_table$age),
            n = max(age_table$age)
          ) %>%
          dplyr::mutate(
            age = round(var)
          ) %>%
          dplyr::filter(
            age %in% age_table$age
          ) %>%
          dplyr::select(
            age, density
          )
      }
      return(res)
    }
  
  data_cp_density <-
    data_source %>%
    dplyr::mutate(
      mvrt_cp_density = purrr::map(
        .x = mvrt_cp,
        .f = ~ get_density_subset(
          data_source = .x,
          age_table = age_table
        )
      ),
      roc_cp_density = purrr::map(
        .x = roc_cp,
        .f = ~ get_density_subset(
          data_source = .x,
          age_table = age_table
        )
      ),
      roc_pp_denisty = purrr::map(
        .x = roc_pp,
        .f = ~ get_density_subset(
          data_source = .x,
          age_table = age_table
        )
      ),
      dcca_cp_density = purrr::map(
        .x = dcca_cp,
        .f = ~ get_density_subset(
          data_source = .x,
          age_table = age_table
        )
      ),
      n0_density = purrr::map(
        .x = diversity_cp,
        .f = ~ .x %>%
          dplyr::filter(var_name == "n0") %>%
          purrr::pluck("age") %>%
          get_density_subset(
            data_source = .,
            age_table = age_table
          )
      ),
      n1_density = purrr::map(
        .x = diversity_cp,
        .f = ~ .x %>%
          dplyr::filter(var_name == "n1") %>%
          purrr::pluck("age") %>%
          get_density_subset(
            data_source = .,
            age_table = age_table
          )
      ),
      n2_density = purrr::map(
        .x = diversity_cp,
        .f = ~ .x %>%
          dplyr::filter(var_name == "n2") %>%
          purrr::pluck("age") %>%
          get_density_subset(
            data_source = .,
            age_table = age_table
          )
      ),
      n1_divided_by_n0_density = purrr::map(
        .x = diversity_cp,
        .f = ~ .x %>%
          dplyr::filter(var_name == "n1_divided_by_n0") %>%
          purrr::pluck("age") %>%
          get_density_subset(
            data_source = .,
            age_table = age_table
          )
      ),
      n2_divided_by_n1_density = purrr::map(
        .x = diversity_cp,
        .f = ~ .x %>%
          dplyr::filter(var_name == "n2_divided_by_n1") %>%
          purrr::pluck("age") %>%
          get_density_subset(
            data_source = .,
            age_table = age_table
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
          roc_pp_denisty, # ..3
          dcca_cp_density, # ..4
          n0_density, # ..5
          n1_density, # ..6
          n2_density, # ..7
          n1_divided_by_n0_density, # ..8
          n2_divided_by_n1_density # ..9
        ),
        .f = ~ age_table %>%
          dplyr::left_join(
            ..1 %>%
              dplyr::select(
                age,
                mvrt = density
              ),
            by = "age"
          ) %>%
          dplyr::left_join(
            ..2 %>%
              dplyr::select(
                age,
                roc = density
              ),
            by = "age"
          ) %>%
          dplyr::left_join(
            ..4 %>%
              dplyr::select(
                age,
                peakpoints = density
              ),
            by = "age"
          ) %>%
          dplyr::left_join(
            ..4 %>%
              dplyr::select(
                age,
                dcca = density
              ),
            by = "age"
          ) %>%
          dplyr::left_join(
            ..5 %>%
              dplyr::select(
                age,
                n0 = density
              ),
            by = "age"
          ) %>%
          dplyr::left_join(
            ..6 %>%
              dplyr::select(
                age,
                n1 = density
              ),
            by = "age"
          ) %>%
          dplyr::left_join(
            ..7 %>%
              dplyr::select(
                age,
                n2 = density
              ),
            by = "age"
          ) %>%
          dplyr::left_join(
            ..8 %>%
              dplyr::select(
                age,
                n1_divided_by_n0 = density
              ),
            by = "age"
          ) %>%
          dplyr::left_join(
            ..9 %>%
              dplyr::select(
                age,
                n2_divided_by_n1 = density
              ),
            by = "age"
          )
      )
    )
  
  return(data_cp_density_merge)
}