# test hypothesis 2 interrelationship in time


data_to_run <- targets::tar_read(data_hvar_filtered,
                                 store = paste0(
                                   data_storage_path,
                                   "_targets_h1"
                                 ))
data_meta <- targets::tar_read(data_meta,
                               store = paste0(
                                 data_storage_path,
                                 "_targets_h1"
                               ))


select_vars <- c("dataset_id","age", "n0", "n1", "n2", "n1_minus_n2", "n2_divided_by_n1" , "n1_divided_by_n0",  "dcca_axis_1", "roc", "density_turnover",  "density_diversity")


# Prepare data - filter out timesteps with less than 3 samples
data_for_h2 <- 
  data_to_run %>% 
  unnest(data_merge) %>% 
  dplyr::select(all_of(select_vars)) %>%
  left_join(data_meta %>% 
              dplyr::select(dataset_id, lat, long, region, ecozone_koppen_15),
            by = "dataset_id") %>%
  drop_na() %>%
  nest(data = -c("age", "ecozone_koppen_15", "region")) %>%
  dplyr::mutate(n_samples = purrr::map_dbl(data, ~nrow(.x))) %>%
  dplyr::filter(n_samples > 4)


# Run PCA analyses for each time bin in regional ecozones; get procrustes sum of square, extract difference with time
pap_procrustes_ecozones <- data_for_h2 %>% 
  mutate(pca_analysis = purrr::map(data,
                                   .f = run_pca)) %>%
  mutate(pca_analysis = pca_analysis %>% 
           rlang::set_names(nm = data_for_h2$age))  %>% 
  group_by(region, ecozone_koppen_15) %>%
  summarise(pca_analysis = list(pca_analysis)) %>% 
  mutate(m2 = purrr::map(pca_analysis, get_procrustes_m2))%>%
  ungroup() %>%
  mutate(m2_time = purrr::map(m2, .f = extract_m2_time))




# Add principal coordinate analysis of similarities and differences with time
pcoa_ecozones <- 
  pap_procrustes_ecozones %>%
  mutate(PCoA = purrr::map(m2, .f = run_pcoa)) %>%
  mutate(site_scores = purrr::pmap(list(PCoA,
                                        region,
                                        ecozone_koppen_15),
                                   .f = ~get_pcoa_scores(pcoa = ..1,
                                                    region = ..2,
                                                    ecozone = ..3))) %>%
  mutate(m2_time_df = purrr::map(m2_time, 
                                 .f = get_m2_time_df))


# plot pcoa diagrams 
pcoa_ecozones %>% 
  dplyr::select(site_scores) %>% 
  unnest(cols = c(site_scores)) %>%
  ggplot(aes(x = X1, y = X2, label = label, col = ecozone)) +
  coord_fixed() +
  geom_hline(yintercept = 0, linetype = "dashed", col = "grey", linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", col = "grey", linewidth = 1) +
  geom_point() + 
  geom_text(hjust = 0, nudge_x = 0.005) +
  labs(x = "PCoA1", y = "PCoA2") +
  facet_wrap(~region)+
  theme_bw() 


# PLOT change in m2 in consecutive time steps
pcoa_ecozones %>%
  dplyr::select(m2_time_df, region, ecozone_koppen_15) %>%
  unnest(cols = c(m2_time_df)) %>%
  ggplot(aes(x = as.numeric(time), y = delta_m2, col = ecozone_koppen_15, fill = ecozone_koppen_15 )) +
  geom_point() +
  geom_smooth() +
  scale_x_reverse() +
  coord_flip() +
  facet_wrap(~region)



