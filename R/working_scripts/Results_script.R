####### SUMMARY OF RESULTS - WORKING SCRIPT
# purpose: wrapper function(s) to be made and included in targets when done 

library(sf)
library(terra)
library(maps)


# Load output/results from targets
# h1
output_h1_temporal <- targets::tar_read(output_hvar_temporal,
                  store = paste0(
                    data_storage_path,
                    "_targets_h1"
                  ))

output_h1_spatial <-  targets::tar_read(output_hvar_spatial,
                                     store = paste0(
                                       data_storage_path,
                                       "_targets_h1"
                                     ))

data_meta <- targets::tar_read(data_meta,
                               store = paste0(
                                 data_storage_path,
                                 "_targets_h1"
                               ))

# h2
output_hvar_h2 <-
  targets::tar_read(
    name = "output_hvar_h2",
    store = paste0(
      data_storage_path,
      "_targets_h2"
    )
  )

# import data for ecoregions (for mapping)
data_geo_koppen <- read_rds(paste0(data_storage_path, 
                                   "Data/ecoregions2017/data_geo_koppen.rds"))




# define color palettes
# 13 colours for ecozones

palette_ecozones <- c(Polar_Frost = "#004949",
                      Polar_Tundra = "#009292",
                      Cold_Without_dry_season = "#006ddb", 
                      Cold_Dry_Summer = "#6db6ff",
                      Cold_Dry_Winter = "#b6dbff",
                      Temperate_Without_dry_season = "#117733",
                      Temperate_Dry_Summer = "#999933" ,
                      Temperate_Dry_Winter =  "#DDCC77",
                      Arid_Desert = "#924900",
                      Arid_Steppe = "#ffff6d",
                      Tropical_Rainforest =  "#920000",
                      Tropical_Monsoon = "#490092",
                      Tropical_Savannah = "#b66dff")

#human vs climate
palette_pred <- c(human = "#663333", 
                  climate = "#BBBBBB") 


###############################################################################
# 1. Prepare results h1
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
# 2. Inspect results h1
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
# 2.2 Filter out poor models
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
 
##########################################################
## 2.3 Display unique variances for humans; bobble maps
#########################################################
# Get the world polygon
world <- map_data("world")

# grey basemap
worldmap_grey <- world %>%
  ggplot() +
  geom_polygon(
    aes(x=long, 
        y = lat, 
        group = group), 
    fill="grey", 
    alpha = 0.4) +
  coord_equal() +
  theme_void()

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
  geom_polygon(data = world, 
               aes(x=long, 
                   y = lat, 
                   group = group), 
               fill="grey", alpha=0.2) +
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
    legend.title = element_text(size = 8),
    panel.spacing=unit(c(0,0,0,0), "null"),
    plot.margin=grid::unit(c(0,0,0,0), "cm"),
    legend.position=c(0.5, 0.1),
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
    legend.title = element_text(size = 8),
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
# 3. Prepare summary tables h1
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




# prepare tables h2:
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

# data for visualisation
data_h2_vis <- data_h2_summary %>%
  pivot_longer(c(Unique_percent, Average.share_percent), 
               names_to = "variance_partition", 
               values_to = "percentage")



##########################################################################
# 5. Visualisation h1 & h2
##########################################################################
# set variables

order_predictors_spatial <- c("human", "climate", "time")
x_label <- c("Human", "Climate", "Time")

# 5.1 OVERVIEW FIG




# 5.2 CIRCULAR BARCHARTS H1 SPATIAL
# spatial input data for h1
select_region = "Latin America"

input_spatial <- summary_spatial_median %>% 
  mutate(ecozone_koppen_15 = factor(ecozone_koppen_15)) %>%
  mutate(predictor = factor(predictor, 
                            levels = order_predictors_spatial)) %>%
  filter(n_records > 5) %>%
  dplyr::filter(region %in% select_region) %>%
  tidyr::complete(ecozone_koppen_15, 
                  nesting(predictor, variance_partition), 
                  fill = list(percentage_median = 0))



circular_bar_h1 <- get_circular_barchart(input_spatial,
                                         y_var = "percentage_median",
                                         title = "")


# Save figure
ggsave(
 paste0("circular_bar_h1_", select_region, ".png"),
 circular_bar_h1,
 width = 3, height = 3, units = "cm",
 scaling = 0.5,
 bg = "white"
)



# 5.3 BARCHARTS H1 TEMPORAL
input_temporal <- 
  summary_temporal_median %>%
  filter(region %in% select_region) %>%
  mutate(predictor =  factor(predictor, 
                             levels = c("human", 
                                        "climate"))) 

bars_temporal_h1 <- get_temporal_barcharts(input_temporal)

ggsave(
  paste0("temporal_bar_", select_region, ".png"),
  bars_temporal_h1,
  width = 6, height = 2, units = "cm",
  scaling = 0.5,
  bg = "transparent"
)

# 5.4 CIRCULAR BARCHART H2

input_h2 <- data_h2_vis %>%
  mutate(group = factor(group)) %>%
  filter(region %in% select_region) %>%
  tidyr::complete(group, 
                  nesting(predictor, variance_partition), 
                  fill = list(percentage = 0))

circular_bar_h2 <- get_circular_barchart(input_h2,
                                         y_var = "percentage",
                                         fill_var = "group",
                                         title = "h2")



# Save figure
ggsave(
  paste0("circular_bar_h2_", select_region, ".png"),
  circular_bar_h2,
  width = 3, height = 3, units = "cm",
  scaling = 0.5,
  bg = "white"
)


# 5.5 VARIOUS MAPS; GLOBAL; REGIONS


map_world_ecozones <- 
  data_geo_koppen %>%
  ggplot() +
  geom_raster(aes(x = x,
                  y = y,
                  fill = ecozone_koppen_15)) +
  scale_fill_manual(values = col_vec, drop = FALSE) +
  coord_sf(expand = TRUE) +
  theme_void() +
  theme(
    legend.position = "bottom",
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.spacing=unit(c(0,0,0,0), "null"),
    plot.margin=grid::unit(c(0,1,0,0), "cm"),
    legend.title = element_blank(),
    legend.text = element_text(size=8),
    legend.key.size = unit(0.5, "cm"),
    legend.margin = margin(0,0,0,0)
    ) + 
  guides(fill = guide_legend(ncol = 3),
             override.aes = list(size = 1)) 

#map_world_ecozones

ecozone_legend <- ggpubr::get_legend(map_world_ecozones)

ggpubr::as_ggplot(ecozone_legend)














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





get_regional_combined_fig(select_region = "Europe")



