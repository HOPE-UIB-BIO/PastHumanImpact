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

# split ecozones
data_meta <- data_meta %>%
  dplyr::mutate(
    sel_classification = dplyr::case_when(
      ecozone_koppen_15 == "Cold_Without_dry_season" ~ ecozone_koppen_30,
      ecozone_koppen_5 == "Cold" ~ ecozone_koppen_15,
      ecozone_koppen_5 == "Temperate" ~ ecozone_koppen_15,
      .default = ecozone_koppen_5
    )
  ) 

# h2
output_hvar_h2 <-
  targets::tar_read(
    name = "output_hvar_h2",
    store = paste0(
      data_storage_path,
      "_targets_h2"
    )
  )

output_hvar_h2$varhp[[17]]

# import data for ecoregions (for mapping)
data_geo_koppen <- read_rds(paste0(data_storage_path, 
                                   "Data/ecoregions2017/data_geo_koppen.rds"))

# add new classification
data_geo_koppen <- 
  data_geo_koppen %>%
  dplyr::mutate(
    sel_classification = dplyr::case_when(
      ecozone_koppen_15 == "Cold_Without_dry_season" ~ ecozone_koppen_30,
      ecozone_koppen_5 == "Cold" ~ ecozone_koppen_15,
      ecozone_koppen_5 == "Temperate" ~ ecozone_koppen_15,
      .default = ecozone_koppen_5
    )
  ) 

# define color palettes
# 13 colours for ecozones


data_meta$sel_classification %>% unique()

           
                                          
                   
palette_ecozones <- 
  c( Polar = "#009292", 
     Cold_Without_dry_season_Very_Cold_Summer = "#004949", 
     Cold_Without_dry_season_Cold_Summer = "#006ddb", 
     Cold_Dry_Winter = "#6db6ff",
     Cold_Dry_Summer = "#b6dbff",
     Cold_Without_dry_season_Warm_Summer = "#117733",
     Cold_Without_dry_season_Hot_Summer = "#999933", 
     Temperate_Without_dry_season = "#DDCC77",
     Temperate_Dry_Winter = "#b66dff",
     Temperate_Dry_Summer = "#ffff6d",
     Arid = "#924900",  
     Tropical =  "#920000"
     )



#human vs climate
palette_pred <- c(human = "#663333", 
                  climate = "#BBBBBB") 


###############################################################################
# 1. Prepare results h1
###############################################################################
# 1.1 Extract results spatial analysis
data_spatial_vis <- 
  output_h1_spatial %>% 
  left_join(data_meta, 
            by = "dataset_id") %>%
  dplyr::select(dataset_id, 
                lat, 
                long, 
                region, 
                sel_classification, 
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
  dplyr::select(-c(data_merge, varhp)) %>%
  rename(p_value = `Pr(>I)`,
         Individual_percent = `I.perc(%)`) %>%
  mutate(across(Unique:Individual_percent, ~replace(., .x < 0, 0))) %>% # negative variances can be ignored 
  mutate(Individual_percent = Individual/total_variance *100) %>% #recalculate individual percent
  group_by(region, sel_classification) %>%
  mutate(n_records = length(unique(dataset_id))) %>%
  ungroup() %>%
  mutate(Unique_percent = Unique/total_variance *100,
        Average.share_percent = Average.share/total_variance *100)  



# 1.2 Extract results temporal analysis
data_temporal_vis <- output_h1_temporal %>%
  dplyr::mutate(summary_table = 
                  purrr::map(varhp, pluck("summary_table"))) %>%
  unnest(summary_table) %>%
  dplyr::mutate(total_variance =
                  purrr::map_dbl(varhp, .f = . %>% pluck("varhp_output") %>% 
                                   pluck("Total_explained_variation"))) %>%
  rename(Individual_percent = `I.perc(%)`) %>%
  mutate(across(Unique:Individual_percent, ~replace(., .x < 0, 0))) %>% 
  mutate(Unique_percent = Unique/total_variance *100,
         Average.share_percent = Average.share/total_variance *100) %>%
  dplyr::select(-c(data_merge, varhp)) %>%
  mutate(p_value = readr::parse_number(`Pr(>I)`)) %>%
  ungroup()


################################################################################
# 2. Filter out models with negative or close to zero adjr2
###############################################################################

# High percentage of individual predictors comes from negative adjusted R2 of the full model;
# 13 datasets
poor_models_spatial <- 
  data_spatial_vis %>% 
  dplyr::filter(total_variance < 0) 

#poor_models_spatial %>% View()


poor_models_spatial_ids <- 
  data_spatial_vis %>% 
  dplyr::filter(total_variance < 0) %>%
  pluck("dataset_id") %>%
  unique()

# remove poor models with negative variance
data_spatial_vis <- data_spatial_vis %>%
  dplyr::filter(!dataset_id %in% poor_models_spatial_ids)


# check range of variation
data_spatial_vis$total_variance %>% boxplot()
data_spatial_vis$total_variance %>% hist()
data_spatial_vis$total_variance %>% range()

# remove models with lower than 5 % quantile from all
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

# how many will be removed
dataset_out <- datasets_total - dataset_in
dataset_out

# check range of p_values
data_spatial_vis$p_value %>% plot()
data_spatial_vis$p_value %>% summary(., na.rm = TRUE)

# filter out datasets with adjr2 lower than 5 percent threshold
dataset_spatial_vis <-
  data_spatial_vis %>%
  dplyr::filter(total_variance > lower_5_percent) 


# |

# check datasets which variation does not add up
dataset_id_check <- data_spatial_vis %>% 
  filter(Individual_percent > 100 | Unique_percent > 100| Average.share_percent > 100) %>% 
  pluck("dataset_id") %>% 
  unique()

check_datasets <- data_spatial_vis %>% 
  filter(dataset_id %in% dataset_id_check) 


# check_datasets %>%
#   group_by(dataset_id) %>%
#   mutate(Individual_recal = case_when(
#     Individual > 0 & Individual < total_variance ~ Individual,
#     Individual > total_variance ~ total_variance,
#     Individual < 0 ~ 0
#   )) %>%
#   mutate(Individual_diff = Individual_recal - (Unique + Average.share)) %>%
#   mutate(Unique_recal = case_when(
#     Unique > Individual_recal  & Average.share == 0 ~ Unique + Individual_diff,
#     Unique > 0 & Unique < Individual_recal ~ Unique, 
#     Unique  > Individual_recal ~ Unique + Average.share,
#     Unique == Individual_recal ~ Unique,
#     Unique < 0 ~ 0)
#     ) %>% 
#   mutate(Average.share_recal = case_when(
#     Unique < 0 & Average.share > Individual_recal ~ Average.share + Unique,
#     Average.share > 0 & Average.share < Individual_recal ~ Average.share,
#     Average.share < 0 ~ 0)
#   ) %>%
#   dplyr::select(dataset_id:Unique, 
#                 Average.share, 
#                 Individual,
#                 Unique_recal,
#                 Average.share_recal , 
#                 Individual_recal, 
#                 Individual_diff, 
#                 total_variance) %>%
# 
#   mutate(Unique_percent = Unique_recal/total_variance *100,
#          Average.share_percent = Average.share_recal/total_variance *100)  %>%
#   View()


# NB: filter out datasets for now with Individual and Unique percentages above 100 = 41 datasets
# sometimes the total_variance is extremely low whereas Individual are high
# comment; there is no satisfcatory solution to readjust the values, and I do not wish to make mistakes by manipulating the results 
data_spatial_vis <- data_spatial_vis %>%
  dplyr::filter(!dataset_id %in% dataset_id_check)


## Check temporal variances
data_temporal_vis$total_variance %>% plot()
data_temporal_vis$total_variance %>% summary(., na.rm = TRUE)
 
##########################################################
## 2.3 Display unique variances for humans; bobble maps
#########################################################

# 
# # plot all data
# bobble_fig_all <- data_spatial_vis %>%
#   dplyr::filter(predictor == "human") %>%
#   mutate(Unique_percent = round(Unique_percent)) %>%
#   ggplot() +
#   geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.2) +
#   geom_point(aes(x = long, 
#                  y = lat, 
#                  color = sel_classification, 
#                  size = Unique_percent), 
#              alpha=0.5) +
#   scale_color_hue(c = 50, l = 60, h = c(30, 300))  +
#   scale_size_continuous(range=c(0,10)) +
#   coord_equal() +
#   theme_void() +
#   theme(
#     panel.spacing=unit(c(0,0,0,0), "null"),
#     plot.margin=grid::unit(c(0,0,0,0), "cm"),
#     legend.position=c(0.3, 0.1),
#     legend.direction="horizontal"
#   ) 
# 
# bobble_fig_all
# 
# # filter variance threshold
# bobble_filter_fig <- 
#   data_spatial_vis %>%
#   dplyr::filter(predictor == "human") %>%
#   dplyr::filter(total_variance > lower_5_percent) %>%
#   mutate(Unique_percent = round(Unique_percent)) %>%
#   ggplot() +
#   geom_polygon(data = world, 
#                aes(x=long, 
#                    y = lat, 
#                    group = group), 
#                fill="grey", alpha=0.2) +
#   geom_point(aes(x = long, 
#                  y = lat, 
#                  color = sel_classification, 
#                  size = Unique_percent), 
#              alpha=0.5) +
#   scale_color_hue(c = 50, l = 60, h = c(30, 300))  +
#   scale_size_continuous(range=c(0,5),
#                         breaks = seq(0,100, by = 20),
#                         limits = c(0, 100)) +
#   coord_equal() +
#   theme_void() +
#   theme(
#     legend.title = element_text(size = 8),
#     panel.spacing=unit(c(0,0,0,0), "null"),
#     plot.margin=grid::unit(c(0,0,0,0), "cm"),
#     legend.position=c(0.5, 0.1),
#     legend.direction="horizontal"
#   ) 
# 
# bobble_filter_fig
# 
# # filter variance threshold and p-value
# bobble_filter2_fig <-
#   data_spatial_vis %>%
#   dplyr::filter(predictor == "human") %>%
#   dplyr::filter(total_variance > lower_5_percent) %>%
#   dplyr::filter(p_value < 0.11) %>%
#   mutate(Unique_percent = round(Unique_percent)) %>%
#   ggplot() +
#   geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.2) +
#   geom_point(aes(x = long, 
#                  y = lat, 
#                  color = sel_classification, 
#                  size = Unique_percent), 
#              alpha=0.5) +
#   scale_color_hue(c = 50, l = 60, h = c(30, 300))  +
#   scale_size_continuous(range=c(0,5),
#                         breaks = seq(0,100, by = 20),
#                         limits = c(0, 100)) +
#   coord_equal() +
#   theme_void() +
#   theme(
#     legend.title = element_text(size = 8),
#     panel.spacing=unit(c(0,0,0,0), "null"),
#     plot.margin=grid::unit(c(0,0,0,0), "cm"),
#     legend.position=c(0.3, 0.1),
#     legend.direction="horizontal"
#   ) 
# 
# bobble_filter2_fig


#############################################################################
# 3. Prepare summary tables h1
#############################################################################


# define variable selection 
group_vars_spatial <- c("predictor", "sel_classification", "region")

group_vars_temporal <- c("predictor", "region", "age")



sel_var <- c("total_variance",
             "Individual", 
             "Unique",
             "Average.share", 
             "Individual_percent", 
             "Unique_percent",
             "Average.share_percent")




# 3.1 Get adjusted R2 summary tables
r2_summary_spatial <-  data_spatial_vis %>%
  get_r2_summary(., group_vars = group_vars_spatial, sel_var = sel_var)


r2_summary_temporal <- data_temporal_vis %>%
  get_r2_summary(., group_vars = group_vars_temporal, sel_var = sel_var)



# 3.2 Extract median values grouped by region & ecozones for spatial or region & age for temporal

# spatial
summary_spatial_median <- 
  r2_summary_spatial %>%
  dplyr::select(predictor, 
                sel_classification, 
                region,
                ends_with("median")) %>%
  pivot_longer(Unique_percent_median:Average.share_percent_median, 
               names_to = "variance_partition", 
               values_to = "percentage_median" ) %>%
  left_join(data_spatial_vis %>% 
              dplyr::select(region, sel_classification, n_records) %>% 
              distinct(), 
            by = c("sel_classification", "region")) %>%
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




# # prepare tables h2:
# data_h2_summary <- output_hvar_h2 %>%
#   dplyr::mutate(summary_table = 
#                   purrr::map(varhp, pluck("summary_table"))) %>%
#   unnest(summary_table) %>%
#   dplyr::mutate(total_variance =
#                   purrr::map_dbl(varhp, .f = . %>% pluck("varhp_output") %>% 
#                                    pluck("Total_explained_variation"))) %>%
#   mutate(across(Unique:`I.perc(%)`, ~replace(., .x < 0, 0))) %>% # negative variances can be ignored 
#   mutate(Unique_percent = Unique/total_variance *100,
#          Average.share_percent = Average.share/total_variance *100) %>%
#   dplyr::select(-c(data_merge, varhp, responce_dist)) %>%
#   ungroup()
# 
# # data for visualisation
# data_h2_vis <- data_h2_summary %>%
#   pivot_longer(c(Unique_percent, Average.share_percent), 
#                names_to = "variance_partition", 
#                values_to = "percentage")



##########################################################################
# 5. Visualisation h1 & h2
##########################################################################
# set variables

order_predictors_spatial <- c("human", "climate", "time")
x_label <- c("Human", "Climate", "Time")



select_region = "North America"

# 5.1 DATA DISTRIBUTION FIG

# data_h1 <- output_h1_spatial %>%
#   left_join(data_meta %>% 
#               dplyr::select(dataset_id, lat, long, region, sel_classification),
#             by = "dataset_id") %>%
#   dplyr::select(-varhp) %>%
#   unnest(data_merge)
# 
# # histogram samples
# data_h1 %>%
#   dplyr::select(dataset_id, age, region, sel_classification, lat, long) %>%
#   group_by(region, sel_classification, age) %>%
#   summarise(
#     n_samples = n()
#   ) %>%
#   ggplot(aes(x = age, y = n_samples, fill = sel_classification)) +
#   geom_histogram(stat = "identity", 
#                  position = position_dodge()) +
#   scale_x_reverse() +
#   coord_flip() +
#   scale_fill_manual(values = palette_ecozones) +
#   facet_wrap(~region, scales = "free")


# data spd

data_h1 %>%
  ggplot(aes(
    x = age, 
    y = spd, 
    fill = sel_classification
  )) + 
  geom_histogram(stat = "identity", 
                 position = position_dodge()) +
  scale_x_reverse() +
  coord_flip() +
  scale_fill_manual(values = palette_ecozones) +
  facet_wrap(~region, scales = "free")






# 5.2 CIRCULAR BARCHARTS H1 SPATIAL
# spatial input data for h1

select_region = "Oceania" 

input_spatial <- 
  summary_spatial_median %>% 
  mutate(sel_classification = factor(sel_classification)) %>%
  mutate(predictor = factor(predictor, 
                            levels = order_predictors_spatial)) %>%
  filter(n_records > 5) %>%
  dplyr::filter(region %in% select_region) %>%
  tidyr::complete(sel_classification, 
                  nesting(predictor, variance_partition), 
                  fill = list(percentage_median = 0))


circular_bar_h1 <- 
  get_circular_barchart(input_spatial,
                        fill_var = "sel_classification",
                        y_var = "percentage_median",
                        title = "")


# Save figure
ggsave(
 paste0("circular_bar_h1_", select_region, ".png"),
 circular_bar_h1,
 width = 3, height = 3, units = "cm",
 scaling = 0.5,
 bg = "transparent"
)


# distribution plots using full dataset

data_dist <- 
  data_spatial_vis %>%
 # dplyr::filter(region %in% select_region) %>%
  mutate(predictor = factor(predictor, 
                            levels = c("time", "climate", "human"))) %>%
  mutate(sel_classification = factor(sel_classification))  %>%
  pivot_longer(c(Unique_percent, Average.share_percent, Individual_percent), 
               names_to = "var_part", 
               values_to = "percentage") %>%
  mutate(var_part = factor(var_part, 
                           levels = c("Unique_percent", 
                                      "Average.share_percent",
                                      "Individual_percent")))

# density of unique, average.share and individual percentage variation for predictors in different regions
data_dist %>%
  ggplot(aes(x = percentage)) +
  geom_density(aes(y = after_stat(count),
                    col = var_part,
                    fill = after_scale(alpha(colour, 0.4)) )
                    ) +
  facet_wrap(~region+ predictor, ncol = 3, scales = "free")


# density of individual, average.share, unique + histogram and data points per ecozone

data_dist %>%
  filter(n_records > 5) %>%
 filter(region %in% select_region) %>%
  ggplot(aes(x = predictor, y  = percentage)) + 
  ggdist::stat_halfeye(
    aes(color = predictor,
        fill = after_scale(lighten(color, .1))),
   # adjust = .5,
    width = .5,
    .width = 0,
    justification = -.7,
    point_colour = NA) +
  geom_boxplot(
    aes(color = sel_classification,
        color = after_scale(darken(color, .1)),
        fill = after_scale(desaturate(lighten(color, .8), .4))
        ),
    width = .6,
    outlier.shape = NA
  ) +
  
  geom_point(
    aes(color = sel_classification,
        color = after_scale(darken(color, .1))),
    fill = "white",
    shape = 21,
    stroke = .4,
    size = 2,
    position = position_dodge(width = .6)
  ) + 
  geom_point(
    aes(fill = sel_classification),
    color = "transparent",
    shape = 21,
    stroke = .4,
    size = 2,
    alpha = .3,
    position = position_dodge(width = .6)
  ) +
  theme(
    legend.position = "none"
  ) +
  scale_y_continuous(
    limits = c(-10, 100),
    expand = c(0, 0),
    breaks = seq(0,100, by = 10)
  ) +
  scale_fill_manual(values = palette_ecozones) +
  scale_color_manual(values = palette_ecozones) +
  coord_flip() +
  labs(x ="", y = "%") +
  facet_wrap(~var_part) 


# # barchart of boxplot and datapoints
# 
# data_dist2 <- 
#   data_spatial_vis %>%
#   dplyr::filter(region %in% select_region) %>%
#   mutate(predictor = factor(predictor, levels = c("time", "climate", "human"))) %>%
#   mutate(sel_classification = factor(sel_classification))  
# 
# data_dist2  %>%
#   ggplot(aes(x = predictor, y  = Unique_percent)) + 
#   #add lines for every 10 percent
#   geom_hline(
#     aes(yintercept = y), 
#     data.frame(y = seq(0, 100, by = 20)),
#     color = "lightgrey"
#   ) + 
#   geom_boxplot(
#     aes(color = sel_classification,
#         fill = sel_classification
#         ),
#     width = .6,
#     outlier.shape = NA,
#     alpha = .5
#   ) +
#   
#   geom_point(
#     aes(color = sel_classification,
#         fill = sel_classification),
#     shape = 21,
#     stroke = .4,
#     size = 2,
#     alpha = 0.5,
#     position = position_dodge(
#       width = .6)
#   ) + 
#  
#   scale_y_continuous(
#     limits = c(-10, 100),
#     expand = c(0, 0),
#     breaks = seq(0,100, by = 10)
#   ) +
#   scale_fill_manual(values = palette_ecozones) +
#   scale_color_manual(values = palette_ecozones) +
#   coord_flip()+
#   coord_polar() +
#   theme_minimal() +
#   geom_segment(
#     aes(x = predictor,
#         y = 0,
#         xend = predictor,
#         yend = 100
#     ),
#     linetype = "dashed",
#     linewidth = 0.3,
#     color = "grey50"
#     
#   ) +
# 
#   theme(
#     legend.position = "none",
#     legend.title = element_text(size = 8),
#     legend.text = element_text(size = 7),
#     legend.key.size = unit(0.2, "cm"),
#     panel.grid = element_blank(),
#     panel.grid.major.x = element_blank(),
#     axis.ticks = element_blank(),
#     axis.text.y = element_blank(),
#     axis.text.x = element_text(colour = "grey30", size = 8, family = "sans", vjust = -3),
#     #axis.title.x = element_text(margin = margin(0, 0, -2, 0)),
#     text = element_text(color = "grey30"),
#     plot.title = element_text(family = "sans",size = 12, hjust = 0.5, margin = margin(0,0,0,0)),
#     plot.margin = unit(c(0.3, 0, 0, 0), "cm")
#   ) +
#   labs(x = "",
#        y = "")






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
select_region <- "Oceania"

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
# # Get the world polygon
# world <- map_data("world")
# 
# # grey basemap
# worldmap_grey <- world %>%
#   ggplot() +
#   geom_polygon(
#     aes(x=long, 
#         y = lat, 
#         group = group), 
#     fill="grey", 
#     alpha = 0.4) +
#   coord_equal() +
#   theme_void()

map_world_ecozones <- 
  data_geo_koppen %>%
  ggplot() +
  geom_raster(aes(x = x,
                  y = y,
                  fill = sel_classification)) +
  scale_fill_manual(values = palette_ecozones, 
                    drop = FALSE) +
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



# combined fig
combined_fig <- get_regional_combined_fig(select_region = select_region)

ggsave(
  paste0("combined_fig_", select_region, ".png"),
  combined_fig,
  width = 8, height = 5, units = "cm",
  scaling = 0.5,
  bg = "white"
)



# # PLOT change in m2 in consecutive time steps
data_m2 <-  targets::tar_read(
  name = "data_m2",
  store = paste0(
    data_storage_path,
    "_targets_h2"
  )
)

#install.packages("vegsoup", repos="http://R-Forge.R-project.org")
cols <- colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"))

cols <- colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"))

vegsoup::coldiss(data_m2$m2[[1]],  byrank = FALSE, diag = TRUE)

data_m2$m2[[1]] %>% as.dist() %>% vegsoup::coldiss(., byrank = FALSE, diag = TRUE)

m2_change_region <- 
  data_m2 %>%
  dplyr::select(m2_time_df, 
                region, 
                sel_classification) %>%
  mutate(region = factor(region, levels = c("North America", 
                                            "Europe", 
                                            "Asia", 
                                            "Latin America", 
                                            "Oceania"))) %>%
  unnest(cols = c(m2_time_df)) %>%
  ggplot(aes(x = as.numeric(time), 
             y = delta_m2, 
             col = sel_classification, 
             fill = sel_classification)) +
  geom_point() +
  geom_smooth() +
  scale_x_reverse() +
  scale_x_continuous(breaks = c(seq(500, 9000, by = 500))) +

  theme_classic() +
  scale_color_manual(values = palette_ecozones, drop = FALSE) +
  scale_fill_manual(values = palette_ecozones, drop = FALSE) +
  theme(
    strip.background = element_blank(),
    legend.position = "none",
    axis.text = element_text(size = 10)) +
  facet_wrap(~region, nrow = 5, scales = "free") +
  labs(x = "Time years BP", y = "change in m2")

ggsave(
  paste0("m2_change_time_regions.png"),
  m2_change_region,
  width = 8, height = 12, units = "cm",
  scaling = 0.5,
  bg = "white"
)



# post hoc procrustes analysis



get_time_protest <- function(data_list){
  
  list_len <- seq_along(data_list) 
  
  vec1 <- (list_len + 1)[-length(list_len)]
  vec2 <- (list_len-1)[-1]
  age <- names(data_list)
  name_list <- paste0(age[vec1],"-",age[vec2])
  
  time_protest <- list()
  
  for(i in seq_along(vec1)) {
    
    time_protest[[i]] <- vegan::protest(X = data_list[[vec1[i]]], 
                                        Y = data_list[[vec2[i]]],
                                        scores = "species",
                                        symmetric = FALSE)  
    
    
  }
  names(time_protest) <- name_list
  
  return(time_protest)
  
}
   
data_m2 <- data_m2 %>%
  mutate(protest_time = purrr::map(pca_analysis, .f = get_time_protest))

data_m2$protest_time[[1]]
         