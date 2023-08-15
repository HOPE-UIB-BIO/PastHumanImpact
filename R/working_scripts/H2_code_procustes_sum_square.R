# test hypothesis 2 interrelationship in time


# data_to_run <- targets::tar_read(data_hvar_filtered,
#                                  store = paste0(
#                                    data_storage_path,
#                                    "_targets_h1"
#                                  ))
data_meta <- targets::tar_read(data_meta,
                               store = paste0(
                                 data_storage_path,
                                 "_targets_h1"
                               ))
# 
# 
# select_vars <- c("dataset_id","age", "n0", "n1", "n2", "n1_minus_n2", "n2_divided_by_n1" , "n1_divided_by_n0",  "dcca_axis_1", "roc", "density_turnover",  "density_diversity")
# 
# 
# # Prepare data - filter out timesteps with less than 3 samples
# data_for_h2 <- 
#   data_to_run %>% 
#   unnest(data_merge) %>% 
#   dplyr::select(all_of(select_vars)) %>%
#   left_join(data_meta %>% 
#               dplyr::select(dataset_id, lat, long, region, ecozone_koppen_15),
#             by = "dataset_id") %>%
#   drop_na() %>%
#   nest(data = -c("age", "ecozone_koppen_15", "region")) %>%
#   dplyr::mutate(n_samples = purrr::map_dbl(data, ~nrow(.x))) %>%
#   dplyr::filter(n_samples > 4)
# 
# 
# # Run PCA analyses for each time bin in regional ecozones; get procrustes sum of square, extract difference with time
# pap_procrustes <- data_for_h2 %>% 
#   mutate(pca_analysis = purrr::map(data,
#                                    .f = run_pca)) %>%
#   mutate(pca_analysis = pca_analysis %>% 
#            rlang::set_names(nm = data_for_h2$age))  %>% 
#   group_by(region, ecozone_koppen_15) %>%
#   summarise(pca_analysis = list(pca_analysis)) %>% 
#   mutate(m2 = purrr::map(pca_analysis, get_procrustes_m2))%>%
#   ungroup() %>%
#   mutate(m2_time = purrr::map(m2, .f = extract_m2_time)) %>%
#   mutate(PCoA = purrr::map(m2, .f = run_pcoa))
#   mutate(m2_time_df = purrr::map(m2_time, 
#                                  .f = get_m2_time_df))
# 
# 
# # plot pcoa diagrams 
# pap_procrustes %>% 
#   mutate(site_scores = purrr::pmap(list(PCoA,
#                                         region,
#                                         ecozone_koppen_15),
#                                    .f = ~get_pcoa_scores(pcoa = ..1,
#                                                          region = ..2,
#                                                          ecozone = ..3))) %>%
#   dplyr::select(site_scores) %>% 
#   unnest(cols = c(site_scores)) %>%
#   ggplot(aes(x = X1, y = X2, label = label, col = ecozone)) +
#   coord_fixed() +
#   geom_hline(yintercept = 0, linetype = "dashed", col = "grey", linewidth = 1) +
#   geom_vline(xintercept = 0, linetype = "dashed", col = "grey", linewidth = 1) +
#   geom_point() + 
#   geom_text(hjust = 0, nudge_x = 0.005) +
#   labs(x = "PCoA1", y = "PCoA2") +
#   facet_wrap(~region)+
#   theme_bw() 
# 
# 
# # PLOT change in m2 in consecutive time steps
# pcoa_ecozones %>%
#   dplyr::select(m2_time_df, region, ecozone_koppen_15) %>%
#   unnest(cols = c(m2_time_df)) %>%
#   ggplot(aes(x = as.numeric(time), y = delta_m2, col = ecozone_koppen_15, fill = ecozone_koppen_15 )) +
#   geom_point() +
#   geom_smooth() +
#   scale_x_reverse() +
#   coord_flip() +
#   facet_wrap(~region)

########################################################################
# Data checking output of Hypothsis 2
#
#######################################################################

# set colours 
# five main ecozones

order_ecozones <- c("Polar_Frost", "Polar_Tundra" ,"Cold_Without_dry_season" ,"Cold_Dry_Summer" , "Cold_Dry_Winter","Temperate_Without_dry_season", "Temperate_Dry_Summer", "Temperate_Dry_Winter" ,  "Arid_Desert" ,"Arid_Steppe" ,      "Tropical_Rainforest" ,"Tropical_Monsoon", "Tropical_Savannah")  

palette_eco <- c("#004949","#009292","#117733", "#DDCC77",
                 "#490092","#006ddb","#b66dff","#6db6ff","#b6dbff",
                 "#920000","#924900","#999933","#ffff6d")

#palette_eco <- c("#222255", "#009988", "#117733", "#DDCC77", "#CC6677") 
#human vs climate
palette_pred <- c("#663333", "#BBBBBB") 

output_hvar_h2 <-
  targets::tar_read(
    name = "output_hvar_h2",
    store = paste0(
      data_storage_path,
      "_targets_h2"
    )
  )


###
output_hvar_h2$data_merge[[1]]

# 1. prepare output for plotting:
data_h2_summary <- output_hvar_h2 %>%
  dplyr::mutate(summary_table = 
                  purrr::map(varhp, pluck("summary_table"))) %>%
  unnest(summary_table) %>%
  dplyr::mutate(total_variance =
                  purrr::map_dbl(varhp, .f = . %>% pluck("varhp_output") %>% 
                                   pluck("Total_explained_variation"))) %>%
  mutate(across(Unique:`I.perc(%)`, ~replace(., .x < 0, 0))) %>% # negative variances can be ignored 
  mutate(Unique_percent = Unique/total_variance *100,
         Average.share_percent = Average.share/total_variance *100) %>%
  dplyr::select(-c(data_merge, varhp)) %>%
  ungroup()

# filter spatial input data


data_h2_vis <- data_h2_summary %>%
  pivot_longer(c(Unique_percent, Average.share_percent), 
               names_to = "variance_partition", 
               values_to = "percentage")


get_regional_combined_fig(select_region = "North America")
get_regional_combined_fig(select_region = "Latin America")



