####### SUMMARY OF RESULTS - WORKING SCRIPT
# purpose: wrapper function(s) to be made and included in targets when done 




# Read targets

output_temporal <- targets::tar_read(output_hvar_temporal,
                  store = paste0(
                    data_storage_path,
                    "_targets_h1"
                  ))

output_spatial <-  targets::tar_read(output_hvar_spatial,
                                     store = paste0(
                                       data_storage_path,
                                       "_targets_h1"
                                     ))

data_meta <- targets::tar_read(data_meta,
                               store = paste0(
                                 data_storage_path,
                                 "_targets_h1"
                               ))

# Import raster data ecozones for visualisation (if maps should display ecozones)

library(sf)
library(terra)
library(maps)

#import data for ecoregions
ecozones_link <- read_csv(paste0(data_storage_path,
                                 "HOPE_Hypothesis1/Data/ecoregions2017/koppen_link.csv"))
ecozone_raster <- terra::rast(paste0(data_storage_path,
                                     "HOPE_Hypothesis1/Data/ecoregions2017/Beck_KG_V1_present_0p083.tif"))



# get data frame of raster data
ecozone_df <- get_rasterdf(ecozone_raster)

# revalue raster & get data for plotting
data_geo_koppen <-
  ecozone_df %>% 
  filter(value > 0) %>%
  dplyr::left_join(ecozones_link,
                   by = c("value" = "raster_values")) %>% 
  dplyr::select(-value) %>% 
  dplyr::rename(
    ecozone_koppen_30 = genzone,
    ecozone_koppen_15 = genzone_cluster,
    ecozone_koppen_5 = broadbiome) 



#save
write_rds(data_geo_koppen, "data_geo_koppen.rds")
#data_geo_koppen <- read_rds("data_geo_koppen.rds")

# define color palettes
# five main ecozones
palette_eco <- c("#222255", "#009988", "#117733", "#DDCC77", "#CC6677") 
#human vs climate
palette_pred <- c("#663333", "#BBBBBB") 

###############################################################################
# 1. PREPARE RESULTS
###############################################################################
# 1.1 Extract results spatial analysis
data_spatial_vis <- output_spatial %>% 
  left_join(data_meta, 
            by = "dataset_id") %>%
  dplyr::select(dataset_id, 
                lat, 
                long, 
                region, 
                ecozone_koppen_15, 
                data_merge, 
                varhp) %>%
  dplyr::mutate(summary_table = 
                  purrr::map(varhp, 
                             pluck("summary_table"))) %>%
  unnest(summary_table) %>%
  dplyr::mutate(total_variance =
                  purrr::map_dbl(varhp, 
                                 .f = . %>% pluck("varhp_output") %>% 
                               pluck("Total_explained_variation"))) %>%
  mutate(across(Unique:`I.perc(%)`, 
                ~replace(., .x < 0, 0))) %>%# negative variances can be ignored
  dplyr::select(-c(data_merge, varhp)) %>%
  mutate(p_value = readr::parse_number(`Pr(>I)`)) %>%
  group_by(region, ecozone_koppen_15) %>%
  mutate(n_records = length(unique(dataset_id))) %>%
  ungroup() %>%
  mutate(Unique_percent = Unique/total_variance *100,
         Average.share_percent = Average.share/total_variance *100)  %>%
  dplyr::select(dataset_id:predictor, 
                total_variance, 
                Unique,
                Average.share,
                Individual,
                Unique_percent,
                Average.share_percent,
                `I.perc(%)`,
                p_value, 
                `Pr(>I)`, 
                n_records)

# 1.2 Extract results temporal analysis
data_temporal_vis <- output_temporal %>%
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
  mutate(p_value = readr::parse_number(`Pr(>I)`)) %>%
  ungroup() %>%
  dplyr::select(age:predictor, 
                total_variance, 
                Unique,
                Average.share,
                Individual,
                Unique_percent,
                Average.share_percent,
                `I.perc(%)`,
                p_value, 
                `Pr(>I)`, 
                n_samples)

################################################################################
# 2. INSPECT RESULTS
###############################################################################

# High percentage of individual predictors comes from negative adjusted R2 of the full model;
# 13 datasets
poor_models_spatial <- data_spatial_vis %>% 
  dplyr::filter(total_variance < 0) 

poor_models_spatial %>% 
  View()

poor_models_spatial_ids <- data_spatial_vis %>% 
  dplyr::filter(total_variance < 0) %>%
  pluck("dataset_id") %>%
  unique()

# 2.1 remove poor models with negative variance
data_spatial_vis <- data_spatial_vis %>%
  dplyr::filter(!dataset_id %in% poor_models_spatial_ids)


############################################################################
# 2.2 Filter models
###########################################################################

data_spatial_vis$total_variance %>% boxplot()
data_spatial_vis$total_variance %>% hist()
data_spatial_vis$total_variance %>% range()

lower_5_percent <- data_spatial_vis$total_variance %>% 
  quantile(., probs = 0.050, na.rm = TRUE) 

# datasets left if threshold of total adjusted r2 is set 
dataset_in <- data_spatial_vis %>%
  dplyr::filter(total_variance > lower_5_percent) %>%
  pluck("dataset_id") %>%
  unique() %>%
  length()

datasets_total <- data_spatial_vis %>%
  pluck("dataset_id") %>%
  unique() %>%
  length()

dataset_out <- datasets_total - dataset_in
dataset_out


data_spatial_vis$p_value %>% plot()
data_spatial_vis$p_value %>% summary(., na.rm = TRUE)


#minimum p-value is 0.04 
 

## 2.3 Check unique variances for humans; bobble maps

# Get the world polygon
world <- map_data("world")

# plot all data
bobble_fig_all <- data_spatial_vis %>%
  dplyr::filter(predictor == "human") %>%
  mutate(Unique_percent = round(Unique_percent)) %>%
  ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.2) +
  geom_point(aes(x = long, 
                 y = lat, 
                 color = ecozone_koppen_15, 
                 size = Unique_percent), 
             alpha=0.5) +
  scale_color_hue(c = 50, l = 60, h = c(30, 300))  +
  scale_size_continuous(range=c(0,10)) +
  coord_equal() +
  theme_void() +
  theme(
    panel.spacing=unit(c(0,0,0,0), "null"),
    plot.margin=grid::unit(c(0,0,0,0), "cm"),
    legend.position=c(0.3, 0.1),
    legend.direction="horizontal"
  ) 

bobble_fig_all

# filter variance threshold
bobble_filter_fig <- 
  data_spatial_vis %>%
  dplyr::filter(predictor == "human") %>%
  dplyr::filter(total_variance > lower_5_percent) %>%
  mutate(Unique_percent = round(Unique_percent)) %>%
  ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.2) +
  geom_point(aes(x = long, 
                 y = lat, 
                 color = ecozone_koppen_15, 
                 size = Unique_percent), 
             alpha=0.5) +
  scale_color_hue(c = 50, l = 60, h = c(30, 300))  +
  scale_size_continuous(range=c(0,5),
                        breaks = seq(0,100, by = 20),
                        limits = c(0, 100)) +
  coord_equal() +
  theme_void() +
  theme(
    panel.spacing=unit(c(0,0,0,0), "null"),
    plot.margin=grid::unit(c(0,0,0,0), "cm"),
    legend.position=c(0.3, 0.1),
    legend.direction="horizontal"
  ) 

bobble_filter_fig

# filter variance threshold and p-value
bobble_filter2_fig <-
  data_spatial_vis %>%
  dplyr::filter(predictor == "human") %>%
  dplyr::filter(total_variance > lower_5_percent) %>%
  dplyr::filter(p_value < 0.11) %>%
  mutate(Unique_percent = round(Unique_percent)) %>%
  ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.2) +
  geom_point(aes(x = long, 
                 y = lat, 
                 color = ecozone_koppen_15, 
                 size = Unique_percent), 
             alpha=0.5) +
  scale_color_hue(c = 50, l = 60, h = c(30, 300))  +
  scale_size_continuous(range=c(0,5),
                        breaks = seq(0,100, by = 20),
                        limits = c(0, 100)) +
  coord_equal() +
  theme_void() +
  theme(
    panel.spacing=unit(c(0,0,0,0), "null"),
    plot.margin=grid::unit(c(0,0,0,0), "cm"),
    legend.position=c(0.3, 0.1),
    legend.direction="horizontal"
  ) 

bobble_filter2_fig

## Check temporal

data_temporal_vis$total_variance %>% plot()
data_temporal_vis$total_variance %>% summary(., na.rm = TRUE)

#############################################################################
# 3. CREATE TABELS FOR SUMMARY FOR ADJUSTED R2
#############################################################################


# define variable selection 
group_vars_spatial <- c("predictor", "ecozone_koppen_15", "region")

group_vars_temporal <- c("predictor", "region", "age")



sel_var <- c("total_variance",
             "Individual", 
             "Unique",
             "Average.share", 
             "I.perc(%)", 
             "Unique_percent",
              "Average.share_percent")




# 3.1 Get adjusted R2 summary tables
r2_summary_spatial <-  data_spatial_vis %>%
  dplyr::filter(total_variance > lower_5_percent) %>%
  get_r2_summary(., group_vars = group_vars_spatial, sel_var = sel_var)


r2_summary_temporal <- data_temporal_vis %>%
  get_r2_summary(., group_vars = group_vars_temporal, sel_var = sel_var)



# 3.2 Extract median values grouped by region & ecozones for spatial or region & age for temporal

# spatial
summary_spatial_median <- 
  r2_summary_spatial %>%
  dplyr::select(predictor, 
                ecozone_koppen_15, 
                region,
                ends_with("median")) %>%
  pivot_longer(Unique_percent_median:Average.share_percent_median, 
               names_to = "variance_partition", 
               values_to = "percentage_median" ) %>%
  left_join(data_spatial_vis %>% 
              dplyr::select(region, ecozone_koppen_15, n_records) %>% 
              distinct(), 
            by = c("ecozone_koppen_15", "region")) %>%
  ungroup()


# temporal
summary_temporal_median <- 
  r2_summary_temporal %>%
  dplyr::select(predictor, 
                age, 
                region,
                ends_with("median")) %>%
  pivot_longer(Unique_percent_median:Average.share_percent_median, 
               names_to = "variance_partition", 
               values_to = "percentage_median" ) %>% 
  left_join(data_temporal_vis %>% 
              dplyr::select(region, age, n_samples) %>% 
              distinct(), 
            by = c("age", "region")) %>%
  ungroup()





##########################################################################
# 4. VISUALISATION OF THE RESULTS
##########################################################################

# 4.1 CIRCULAR BARCHARTS SPATIAL


# check colors for colorblindness
colorblindr::cvd_grid()


# set variables
select_region = "Europe"


order_ecozones <- c("Polar_Frost", "Polar_Tundra" ,"Cold_Without_dry_season" ,"Cold_Dry_Summer" , "Cold_Dry_Winter","Temperate_Without_dry_season", "Temperate_Dry_Summer", "Temperate_Dry_Winter" ,  "Arid_Desert" ,"Arid_Steppe" ,      "Tropical_Rainforest" ,"Tropical_Monsoon", "Tropical_Savannah")  

palette_eco <- c("#004949","#009292","#117733", "#DDCC77",
                 "#490092","#006ddb","#b66dff","#6db6ff","#b6dbff",
                 "#920000","#924900","#999933","#ffff6d")

order_predictors_spatial <- c("human", "climate", "time")

x_label <- c("Human", "Climate", "Time")

# filter spatial input data
input_spatial <- summary_spatial_median %>% 
  mutate(predictor = factor(predictor, 
                            levels = order_predictors_spatial)) %>%
  mutate(ecozone_koppen_15 = factor(ecozone_koppen_15, 
                                   levels = order_ecozones)) %>%
  dplyr::filter(region %in% select_region) %>%
  filter(n_records > 5)

input_spatial$ecozone_koppen_15 %>% levels()

circular_bar_fig <- get_circular_barchart(input_spatial, 
                                          y_var = "percentage_median",
                                          fill_var = "ecozone_koppen_15")
circular_bar_fig

## Save figure
# ggsave(
#  paste0("circular_bar_", select_region, ".png"),
#  circular_bar_fig,
#  width = 3, height = 3, units = "cm",
#  scaling = 0.5,
#  bg = "transparent"
# )

# 4.2 BARCHART TEMPORAL 

# filter temporal input data
input_temporal <- 
  summary_temporal_median %>%
  filter(region %in% select_region) %>%
  mutate(predictor =  factor(predictor, 
                          levels = c("human", "climate"))) 

bars_temporal_fig <- get_temporal_barcharts(input_temporal)
bars_temporal_fig



# ggsave(
#   paste0("temporal_bar_", select_region, ".png"),
#   bars_temporal_fig,
#   width = 6, height = 2, units = "cm", 
#   scaling = 0.5,
#   bg = "transparent"
# )


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

# Function for report summary
get_regional_combined_fig(select_region = "North America")


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


get_regional_combined_fig(select_region = "Europe")



