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
data_h2_vis <- output_hvar_h2 %>%
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
select_region <- "Latin America"

data_to_plot <- data_h2_vis %>%
  pivot_longer(c(Unique_percent,Average.share_percent), 
               names_to = "variance_partition", 
               values_to = "percentage") %>%
  filter(region == select_region) 






data_to_plot %>%
  ggplot() + 
  #add lines for every 10 percent
  geom_hline(
    aes(yintercept = y), 
    data.frame(y = seq(0, 50, by = 10)),
    color = "lightgrey"
  ) +
  geom_col(data = data_to_plot %>% 
             dplyr::filter(variance_partition == "Unique_percent"), 
           aes(x = predictor,
               y = percentage,
               fill = group),
           position_dodge(width = 0.9), alpha = 1) +
  geom_col(data = data_to_plot %>% 
             dplyr::filter(variance_partition == "Average.share_percent"),
           aes(x = predictor,
               y = percentage,
               fill = group),
           position = position_dodge(width = 0.9), alpha = 0.4) +
 # scale_fill_manual(values = fill_eco, 
 #                    drop = FALSE) +
  geom_segment(
    aes(x = predictor,
        y = 0,
        xend = predictor,
        yend = 50
    ),
    linetype = "dashed",
    linewidth = 0.3,
    color = "grey50"
    
  ) +
  scale_y_continuous(
    limits = c(-10, 50),
    expand = c(0, 0),
    breaks = c(0, 10,20, 30, 40, 50)
  ) +
  annotate("text",
           x = rep(seq(1,3, by = 1),4),
           y = rep(seq(10,40, by = 10),3),
           label = rep(paste0(seq(10,40, by = 10), " %"),3), 
           vjust = 0,
           size = 3) +
  coord_polar() +
  theme_minimal()+
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(colour = "grey30", size = 10, family = "sans", vjust = -3),
    #axis.title.x = element_text(margin = margin(0, 0, -2, 0)),
    text = element_text(color = "grey30"),
    plot.title = element_text(family = "sans",size = 12, hjust = 0.5, margin = margin(0,0,0,0)),
    plot.margin = unit(c(0.3, 0, 0, 0), "cm")
  ) +
 # scale_x_discrete(label = x_name, 
 #                   drop = FALSE) +
  labs( 
    # title = paste0(select_region),
    x = "", 
    y = ""
  ) 

# temporal bar 
# filter temporal input data
input_temporal <- 
  data_temporal %>%
  filter(region %in% select_region) %>%
  mutate(predictor =  factor(predictor, 
                             levels = c("human", "climate"))) 

bars_temporal_fig <- get_temporal_barcharts(input_temporal)

# Regional maps
map_region <- get_map_region(select_region = select_region)

# Combine figures 
final <- 
  ggpubr::ggarrange(
    ggpubr::ggarrange(
      circular_bar_fig,
      ggpubr::ggarrange(map_region, 
                        NULL, 
                        ncol = 1,
                        heights = c(2,1)
      ), 
      ncol = 2,
      widths = c(2,1)),
    bars_temporal_fig,
    nrow = 2,
    heights = c(2,1)
  )

final

