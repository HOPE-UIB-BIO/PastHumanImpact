#' @title Get multivariate regression trees (MRT) 
#' @description Use pollen assemblage data and age/time as constraint
#' @param data_pollen Use pollen percentages
#' @param n_rand number of randomisations
#' @param transformation_coef which distance coefficient to use. Recommended for pollen data is chi-squared distances of pollen percentage with no data transformations  
#' @return For each sequence returns a vector of the zonation, a vector of the change points, and the total number of zones 

get_mrt <- function(data_pollen, n_rand = 999, transformation_coef = "chisq") {
  
  data_work_mrt <-
    data_pollen %>%
    dplyr::mutate(
      PAP_mrt = purrr::map2(
        .x = pollen_percentages,
        .y = levels,
        .f = ~ REcopol::mv_regression_partition(
          data_source_counts = .x,
          data_source_levels = .y,
          rand = n_rand, 
          transformation = transformation_coef 
        )
      )
    )
  
  data_mrt_proc <-
    data_work_mrt %>%
    dplyr::mutate(
      mvrt_partitions = purrr::map(
        .x = PAP_mrt,
        .f = ~ .x %>%
          purrr::pluck("partitions") %>%
          dplyr::rename(MRT_partitions = partition)
      ),
      mvrt_cp = purrr::map(
        .x = PAP_mrt,
        .f = ~ .x %>%
          purrr::pluck("change_points")
      ),
      mvrt_groups_n = purrr::map_dbl(
        .x = PAP_mrt,
        .f = ~ .x %>%
          purrr::pluck("mrt_groups") 
      )
    )
  
  data_mrt <-
    data_mrt_proc %>%
    dplyr::select(dataset_id, PAP_mrt, mvrt_partitions, mvrt_cp, mvrt_groups_n)
  
  return(data_mrt)
}