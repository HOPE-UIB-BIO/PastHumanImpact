filter_data_hvar <- function(data_source, 
                             data_meta, 
                             age_table) {
  data_source %>%
    unnest(data_merge) %>%
    drop_na() %>%
    left_join(data_meta %>% 
                dplyr::select(dataset_id, end_of_interest_period),
              by = "dataset_id") %>%
    left_join(age_table, by = "region") %>%
    filter(age >= age_from & age <= end_of_interest_period) %>%
    filter(!region == "Africa") %>%
    dplyr::select(-c(age_from, end_of_interest_period, region)) %>%
    nest(data_merge = -c(dataset_id)) %>%
    return()
  
}