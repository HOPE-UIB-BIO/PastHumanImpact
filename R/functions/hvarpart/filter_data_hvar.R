filter_data_hvar <- function(data_source, 
                             data_meta, 
                             age_table) {
  data_source %>%
    unnest(data_merge) %>%
    drop_na() %>%
    left_join(data_meta %>% 
                dplyr::select(dataset_id, region, data_publicity),
              by = "dataset_id") %>%
    left_join(age_table, by = "region") %>%
    dplyr::filter(!(region == "Latin America" & data_publicity == "private")) %>%
    filter(!region == "Africa") %>%
    filter(age >= age_from & age <= age_end) %>%
    dplyr::select(-c(age_from, age_end, region, data_publicity)) %>%
    nest(data_merge = -c(dataset_id)) %>%
    return()
  
}
