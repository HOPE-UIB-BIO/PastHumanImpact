#############################################################
## VISUALISATION
## FIGURE 3: RESULTS H2
###########################################################
# Import results & tables for plotting
library(patchwork)

source("R/working_scripts/Results_script.R")

# temporarily quick fix: rerun m2 with sel_classification
data_for_hvar <-  
  targets::tar_read(
    name = "data_for_hvar",
    store = paste0(
      data_storage_path,
      "_targets_h1"
    )
  )

data_for_m2 <- 
  data_for_hvar %>%
  inner_join(data_meta %>% 
               dplyr::select(dataset_id, 
                             region, 
                             sel_classification), 
             by ="dataset_id")


vec_responses <-
  c(
    "dataset_id",
    "age",
    "region",
    "sel_classification",
    "n0",
    "n1",
    "n2",
    "n1_minus_n2",
    "n2_divided_by_n1",
    "n1_divided_by_n0",
    "dcca_axis_1", "roc",
    "density_turnover",
    "density_diversity"
  )

data_m2 <- 
  get_data_m2(
    data_source = data_for_hvar,
    data_meta = data_meta,
    min_samples = 5,
    select_vars = vec_responses
  )


# Import data for mapping
data_geo_koppen <- 
  read_rds(paste0(data_storage_path, 
                  "Data/ecoregions2017/data_geo_koppen.rds")
           )

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

# Define color palettes for ecozones
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



palette_polar <- 
  c( Polar = "#009292", 
     Cold_Without_dry_season_Very_Cold_Summer = "grey80", 
     Cold_Without_dry_season_Cold_Summer = "grey80", 
     Cold_Dry_Winter = "grey80",
     Cold_Dry_Summer = "grey80",
     Cold_Without_dry_season_Warm_Summer = "grey80",
     Cold_Without_dry_season_Hot_Summer = "grey80", 
     Temperate_Without_dry_season = "grey80",
     Temperate_Dry_Winter = "grey80",
     Temperate_Dry_Summer = "grey80",
     Arid = "grey80",  
     Tropical =  "grey80"
  )


palette_cold <- 
  c( Polar = "grey80", 
     Cold_Without_dry_season_Very_Cold_Summer = "#004949", 
     Cold_Without_dry_season_Cold_Summer = "#006ddb", 
     Cold_Dry_Winter = "#6db6ff",
     Cold_Dry_Summer = "#b6dbff",
     Cold_Without_dry_season_Warm_Summer = "#117733",
     Cold_Without_dry_season_Hot_Summer = "#999933", 
     Temperate_Without_dry_season = "grey80",
     Temperate_Dry_Winter = "grey80",
     Temperate_Dry_Summer = "grey80",
     Arid = "grey80",  
     Tropical =  "grey80"
  )


palette_temperate <- 
  c( Polar = "grey80", 
     Cold_Without_dry_season_Very_Cold_Summer =  "grey80", 
     Cold_Without_dry_season_Cold_Summer =  "grey80", 
     Cold_Dry_Winter =  "grey80",
     Cold_Dry_Summer =  "grey80",
     Cold_Without_dry_season_Warm_Summer =  "grey80",
     Cold_Without_dry_season_Hot_Summer =  "grey80", 
     Temperate_Without_dry_season = "#DDCC77",
     Temperate_Dry_Winter = "#b66dff",
     Temperate_Dry_Summer = "#ffff6d",
     Arid =  "grey80",  
     Tropical =   "grey80"
  )

palette_arid <- 
  c( Polar = "grey80", 
     Cold_Without_dry_season_Very_Cold_Summer =  "grey80", 
     Cold_Without_dry_season_Cold_Summer =  "grey80", 
     Cold_Dry_Winter =  "grey80",
     Cold_Dry_Summer =  "grey80",
     Cold_Without_dry_season_Warm_Summer =  "grey80",
     Cold_Without_dry_season_Hot_Summer =  "grey80", 
     Temperate_Without_dry_season = "grey80",
     Temperate_Dry_Winter = "grey80",
     Temperate_Dry_Summer = "grey80",
     Arid =  "#924900",  
     Tropical =   "grey80"
  )

palette_tropical <- 
  c( Polar = "grey80", 
     Cold_Without_dry_season_Very_Cold_Summer =  "grey80", 
     Cold_Without_dry_season_Cold_Summer =  "grey80", 
     Cold_Dry_Winter =  "grey80",
     Cold_Dry_Summer =  "grey80",
     Cold_Without_dry_season_Warm_Summer =  "grey80",
     Cold_Without_dry_season_Hot_Summer =  "grey80", 
     Temperate_Without_dry_season = "grey80",
     Temperate_Dry_Winter = "grey80",
     Temperate_Dry_Summer = "grey80",
     Arid =  "grey80",  
     Tropical =   "#920000"
  )

# Predictors
palette_pred <- c(human = "#663333", 
                  climate = "#BBBBBB") 

# Set parameters

order_predictors_spatial <- c("human", "time",  "climate")

x_label <- c("Human",  "Time", "Climate")


# GET LISTS OF REGIONAL MAPS FOR DIFFERENT PALETTE


list_maps <- function(select_region = "Europe") {
  
  map_region_polar <- get_map_region(select_region = select_region,
                                     col_vec = palette_polar)
  map_region_cold <- get_map_region(select_region = select_region,
                                    col_vec = palette_cold)
  map_region_temp <- get_map_region(select_region = select_region,
                                    col_vec = palette_temperate)
  map_region_arid <- get_map_region(select_region = select_region,
                                    col_vec = palette_arid)
  map_region_tropical <- get_map_region(select_region = select_region,
                                        col_vec = palette_tropical)
  
  map_list <- list(polar = map_region_polar, 
                   cold = map_region_cold, 
                   temp = map_region_temp, 
                   arid = map_region_arid,
                   tropical = map_region_tropical)
  
  return(map_list)
  
  
}


NA_map_list <- list_maps(select_region = "North America")


LA_map_list <- list_maps(select_region = "Latin America")

Europe_map_list <- list_maps(select_region = "Europe")

Asia_map_list <- list_maps(select_region = "Asia")

oceania_map_list <- list_maps(select_region = "Oceania")


# GET FIGURES CIRCULAR BARS
circular_bar_h2 <- 
  data_h2_vis %>%
  mutate(group = factor(group)) %>%  
  group_by(region) %>%
  group_map(~ get_circular_barplot(.x,
                                   y_var = "percentage",
                                   fill_var = "group")
  ) 

# name list
names(circular_bar_h2) <- 
  data_h2_vis$region %>% 
  unique()

# GET FIGURES CONSECUTIVE PROCRUSTES CHANGE WITH TIME

m2_change_region <- 
  data_m2 %>%
  dplyr::select(m2_time_df, 
                region, 
                sel_classification) %>%
  filter(!region == "Africa") %>%
  inner_join(data_meta %>% 
               dplyr::select(region, sel_classification, ecozone_koppen_5) %>%
               distinct(), 
             by = c("region", "sel_classification")) %>%
  unnest(cols = c(m2_time_df)) %>%
  group_by(region, ecozone_koppen_5) %>%
  group_map(~ ggplot(data = .x, aes(x = as.numeric(time), 
                                    y = delta_m2, 
                                    col = sel_classification, 
                                    fill = sel_classification)) +
              geom_point(size = 0.5) +
              geom_smooth(linewidth = 0.1, se = FALSE) +
              scale_x_reverse() +
              scale_x_continuous(
                limits = c(500, 8500),
                breaks = c(seq(500, 8500, by = 2000))
              ) +
              scale_y_continuous(limits = c(0, 1))+
              theme_minimal() +
              scale_color_manual(
                values = palette_ecozones, 
                drop = FALSE
              ) +
              scale_fill_manual(
                values = palette_ecozones, 
                drop = FALSE
              ) +
              theme(
                aspect.ratio = 1,
                legend.position = "none",
                panel.background  = element_blank(),
                strip.background = element_blank(),
                panel.grid.minor = element_blank(),
                plot.background = element_rect(
                  fill = "transparent", 
                  color = NA), 
                panel.grid.major = element_line(
                  color = "grey90", 
                  linewidth = 0.1), 
                axis.title.y = element_text(size = 6),
                axis.text.x = element_text(size = 6, angle = 60),
                axis.text.y = element_text(size = 6),
                plot.margin = unit(c(0,0,0,0), "cm")
              ) +
              labs(x = "", 
                   y = "change in m2"
              )
  )

# name list correct
groups <- 
  data_m2 %>%
  dplyr::select(m2_time_df, 
                region, 
                sel_classification) %>%
  filter(!region %in% "Africa") %>%
  inner_join(data_meta %>% 
               dplyr::select(region, sel_classification, ecozone_koppen_5) %>%
               distinct(), 
             by = c("region", "sel_classification")) %>%
  unnest(cols = c(m2_time_df)) %>%
  group_by(region, ecozone_koppen_5) %>%
  attr("groups")

name_list <- paste0(groups$region, "_",groups$ecozone_koppen_5)

names(m2_change_region) <- name_list

# COMBINE FIGURES


# to shift themes


remove_axis_labels <-  theme(axis.title.x = element_blank(),
                      axis.text.x = element_blank(),
                      axis.title.y = element_blank(),
                      axis.text.y = element_blank())

remove_axis_labels_x <- theme(axis.title.x = element_blank(),
                              axis.text.x = element_blank())

remove_axis_labels_y <- theme(axis.title.y = element_blank(),
                             axis.text.y = element_blank())

remove_space <- theme(plot.margin = unit(c(0,-2,0,-2), "mm"))

# PUT TOGETHER FIGURES PER REGION


dev.new(width = 180, height = 70, units = "mm") 




NA_p1 <-
  circular_bar_h2$`North America` +
  m2_change_region$`North America_Arid` + 
  remove_axis_labels_x +
  inset_element(NA_map_list$arid, left = 0.365, bottom = 0.7, right = 1, top = 1) +
  m2_change_region$`North America_Polar` +
  remove_axis_labels +
  remove_space +
  inset_element(NA_map_list$polar, left = 0.365, bottom = 0.7, right = 1, top = 1) +  
  m2_change_region$`North America_Cold` +
  remove_axis_labels +
  remove_space +
  inset_element(NA_map_list$cold, left = 0.365, bottom = 0.7, right = 1, top = 1) +
  m2_change_region$`North America_Temperate` +
  remove_axis_labels +
  remove_space +
  inset_element(NA_map_list$temp, left = 0.365, bottom = 0.7, right = 1, top = 1) +
  plot_layout(nrow = 1)



LA_p1 <- 
  circular_bar_h2$`Latin America` +
  m2_change_region$`Latin America_Arid` + 
  remove_axis_labels_x +
  inset_element(LA_map_list$arid, left = 0.365, bottom = 0.7, right = 1, top = 0.9) +
  m2_change_region$`Latin America_Polar` +
  remove_axis_labels +
  remove_space +
  inset_element(LA_map_list$polar, left = 0.365, bottom = 0.7, right = 1, top = 0.9) +
  m2_change_region$`Latin America_Temperate` +
  remove_axis_labels +
  remove_space +
  inset_element(LA_map_list$temp, left = 0.365, bottom = 0.7, right = 1, top = 0.9) +
  m2_change_region$`Latin America_Tropical` +
  remove_axis_labels +
  remove_space +
  inset_element(LA_map_list$tropical, left = 0.365, bottom = 0.7, right = 1, top = 0.9) +
  plot_layout(nrow = 1)
 

europe_p1 <- 
  circular_bar_h2$`Europe` +
  m2_change_region$`Europe_Arid` + 
  remove_axis_labels_x +
  inset_element(Europe_map_list$arid, left = 0.365, bottom = 0.7, right = 1, top = 1) +
  m2_change_region$`Europe_Polar` +
  remove_axis_labels +
  remove_space +
  inset_element(Europe_map_list$polar, left = 0.365, bottom = 0.7, right = 1, top = 1) +
  m2_change_region$`Europe_Temperate` +
  remove_axis_labels +
  remove_space +
  inset_element(Europe_map_list$temp, left = 0.365, bottom = 0.7, right = 1, top = 1) +
  m2_change_region$`Europe_Cold` +
  remove_axis_labels +
  remove_space +
  inset_element(Europe_map_list$cold, left = 0.365, bottom = 0.7, right = 1, top = 1) +
  plot_layout(nrow = 1)



asia_p1 <- 
  circular_bar_h2$`Asia` +
  m2_change_region$`Asia_Arid` + 
  inset_element(Asia_map_list$arid, left = 0.365, bottom = 0.7, right = 1, top = 1) +
  m2_change_region$`Asia_Polar` +
  remove_axis_labels_y +
  remove_space +
  inset_element(Asia_map_list$polar, left = 0.365, bottom = 0.7, right = 1, top = 1) +
  m2_change_region$`Asia_Temperate` +
  remove_axis_labels +
  remove_space +
  inset_element(Asia_map_list$temp, left = 0.365, bottom = 0.7, right = 1, top = 1) +
  m2_change_region$`Asia_Cold` +
  remove_axis_labels +
  remove_space +
  inset_element(Asia_map_list$cold, left = 0.365, bottom = 0.7, right = 1, top = 1) +
  plot_layout(nrow = 1)

 

oceania_p1 <- 
  circular_bar_h2$`Oceania` +
  plot_spacer() +
  plot_spacer() +
  m2_change_region$`Oceania_Temperate` +
  remove_axis_labels_y +
  inset_element(oceania_map_list$temp, left = 0.365, bottom = 0.7, right = 1, top = 0.99) +
  plot_spacer() +
  plot_layout(nrow = 1)

# FINAL COMBINED
layout <- c(
  area(t = 1, l = 1, b = 1, r = 5),
  area(t = 2, l = 1, b = 2, r = 5),
  area(t = 3, l = 1, b = 3, r = 5),
  area(t = 4, l = 1, b = 4, r = 5),
  area(t = 5, l = 1, b = 5, r = 5)
)

plot(layout)


combine_plots <- 
 (NA_p1 / 
  LA_p1 / 
  europe_p1 /
  asia_p1 / 
  oceania_p1) + 
  plot_layout(design = layout)


# FINAL COMBINE ALL REGIONS
combine_h2 <-
  cowplot::plot_grid(NA_p1, 
                     LA_p1, 
                     europe_p1, 
                     asia_p1, 
                     oceania_p1, 
                     ncol = 1, 
                     align = "hv",
                     axis = 1) 
  
combine_h2

dev.off()

# SAVE

pdf("combined_h2_version3.pdf", width = 9, height = 9, unit = "cm")  
print(combine_h2)  
dev.off()

system2('open', args = 'combined_h2_version3.pdf')

png("combined_plot_h2.png", width = 9, height = 9, unit = "cm", dpi = 300)
print(combined)

dev.off()

system2('open', args = 'combined_h2_version3.pdf')

ggsave(
  "combined_figure_h2.png",
  combine_h2,
  width = 18, height = 7.5, units = "cm",
  dpi = 500,
  bg = "white"
)

