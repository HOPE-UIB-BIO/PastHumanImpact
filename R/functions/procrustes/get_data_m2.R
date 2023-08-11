get_data_m2 <- function(data_source = data_for_hvar,
                        data_meta,
                        min_samples = 5,
                        select_vars = NULL
                        ) {
  
  # prepare data
  data_for_h2 <- 
    data_source %>% 
    unnest(data_merge) %>% 
    dplyr::select(all_of(select_vars)) %>%
    left_join(data_meta %>% 
                dplyr::select(dataset_id, lat, long, region, ecozone_koppen_15),
              by = "dataset_id") %>%
    drop_na() %>%
    nest(data = -c("age", "ecozone_koppen_15", "region")) %>%
    dplyr::mutate(n_samples = purrr::map_dbl(data, ~nrow(.x))) %>%
    dplyr::filter(n_samples >= min_samples)
  
  # Run PCA analyses; get procrustes sum of square matrices; extract difference with time
  pap_procrustes <- data_for_h2 %>% 
    mutate(pca_analysis = purrr::map(data,
                                     .f = run_pca)) %>%
    mutate(pca_analysis = pca_analysis %>% 
             rlang::set_names(nm = data_for_h2$age))  %>% 
    group_by(region, ecozone_koppen_15) %>%
    summarise(pca_analysis = list(pca_analysis)) %>% 
    mutate(m2 = purrr::map(pca_analysis, get_procrustes_m2))%>%
    ungroup() %>%
    mutate(m2_time = purrr::map(m2, .f = extract_m2_time)) %>%
    mutate(PCoA = purrr::map(m2, .f = run_pcoa))
    mutate(m2_time_df = purrr::map(m2_time, 
                                 .f = get_m2_time_df))
  
  return(pap_procrustes)
  
  
}
  
