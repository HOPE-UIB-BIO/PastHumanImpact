#' @title Filter pollen sequences of from start of relevant time period 
#' @description Filter samples based on latest age of interest
#' @return the same input data tibble but with filtered pollen data

filter_age_levels <- function(data, late_age_limit = 8.5e3) {
  
  data %>%
    mutate(sample_ids_keep = 
             purrr::map2(levels, 
                         end_of_interest_period,
                         .f = function(.x, .y){
                           sample_id <- .x %>%
                             filter(age < .y) %>%
                             pluck("sample_id")
                         })) %>%
    mutate(across(c(levels, counts_harmonised, pollen_percentages),
                  .fns = ~ 
                    map2(.x, .y = sample_ids_keep,
                         .f = function(.x, .y){
                           .x %>% 
                             filter(sample_id %in% .y)
                         } ))) %>%
    dplyr::select(-sample_ids_keep)
  
}