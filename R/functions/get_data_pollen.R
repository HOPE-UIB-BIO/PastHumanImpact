#' @title Get pollen data from data assembly
#' @description Use data assembly derived from RFossilPol
#' @return For each site a tibble with dataset_id, levels, harmonised pollen counts and pollen percentages

get_data_pollen <- function(file) {
  
  read_rds(file) %>% 
    pluck("data") %>%
    dplyr::select(dataset_id, 
                  levels, 
                  counts_harmonised, 
                  pollen_percentage,
                  age_uncertainty,
                  end_of_interest_period) %>%
    dplyr::mutate(
      pollen_percentages = 
        purrr::map(
          .x = counts_harmonised,
          .f = function(.x) {
            df <-  .x %>%
              column_to_rownames("sample_id")
            percent <- df/rowSums(df) *100
            new <- percent %>% 
              rownames_to_column("sample_id") %>% 
              as_tibble()
            return(new)
          }
        ))
}