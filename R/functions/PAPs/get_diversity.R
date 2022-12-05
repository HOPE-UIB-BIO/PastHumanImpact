#' @title Get diversity estimates
#' @description Use pollen assemblage data
#' @param data_pollen use pollen counts if possible
#' @param n_rand number of randomisations
#' @param sel_method selection of approach
#' @return For each site dataset the rarefied estimates of Hill´s effective
#' species numbers (which are the estimated number of taxa (N0), estimated
#' number of equally common taxa (N1), estimated number of equally abundant
#' taxa (N2)), and the associated evenness ratios N1/N0 and N2/N1

get_diversity <- function(data_pollen,
                          n_rand = 999,
                          sel_method = "taxonomic") {
  data_work_diversity <-
    data_pollen %>%
    dplyr::mutate(
      PAP_diversity = purrr::map(
        .x = counts_harmonised,
        .f = ~ REcopol::diversity_estimate(
          data_source = .x,
          sel_method = sel_method,
          rand = n_rand
        )
      )
    )

  data_diversity <-
    data_work_diversity %>%
    dplyr::select(dataset_id, PAP_diversity)

  return(data_diversity)
}
