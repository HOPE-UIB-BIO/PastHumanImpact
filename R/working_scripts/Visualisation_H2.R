#############################################################
## VISUALISATION
## FIGURE 3: RESULTS H2
###########################################################
# Import results & tables for plotting

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
select_region = "North America"

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

NA_map_list <- list(polar = map_region_polar, 
                    cold = map_region_cold, 
                    temp = map_region_temp, 
                    arid = map_region_arid,
                    tropical = map_region_tropical)


LA_map_list <- list(polar = map_region_polar, 
                    cold = map_region_cold, 
                    temp = map_region_temp, 
                    arid = map_region_arid,
                    tropical = map_region_tropical)

Europe_map_list <- list(polar = map_region_polar, 
                        cold = map_region_cold, 
                        temp = map_region_temp, 
                        arid = map_region_arid,
                        tropical = map_region_tropical)

Asia_map_list <- list(polar = map_region_polar, 
                      cold = map_region_cold, 
                      temp = map_region_temp, 
                      arid = map_region_arid,
                      tropical = map_region_tropical)

oceania_map_list <- list(polar = map_region_polar, 
                         cold = map_region_cold, 
                         temp = map_region_temp, 
                         arid = map_region_arid,
                         tropical = map_region_tropical)


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
                  linewidth = 0.01), 
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



circular_bar_h2 %>% 
  names()



# Vectors to extract regions from lists

NorthA <- 
  m2_change_region %>% 
  names() %>% 
  grepl("North America", .)

LatinA <- 
  m2_change_region %>% 
  names() %>% 
  grepl("Latin America", .)

Europe <- 
  m2_change_region %>% 
  names() %>% 
  grepl("Europe", .)

Asia <- 
  m2_change_region %>% 
  names() %>% 
  grepl("Asia", .)

Oceania <- 
  m2_change_region %>% 
  names() %>% 
  grepl("Oceania", .)

# PUT TOGETHER FIGURES PER REGION

NA_p1 <- 
  cowplot::plot_grid(
    circular_bar_h2[["North America"]],
    plotlist = m2_change_region[NorthA], 
    nrow = 1,
    rel_widths = c(2, 1, 1, 1, 1))

NA_h2 <-
  cowplot::ggdraw(NA_p1) +
  cowplot::draw_plot(NA_map_list$arid, 
                     x = 0.365, 
                     y = 0.7, 
                     width = .2, 
                     height = .2) +
  cowplot::draw_plot(NA_map_list$cold, 
                     x = 0.535, 
                     y = 0.7, 
                     width = .2, 
                     height = .2) +
  cowplot::draw_plot(NA_map_list$polar, 
                     x = 0.7, 
                     y = 0.7, 
                     width = .2, 
                     height = .2) +
  cowplot::draw_plot(NA_map_list$temp, 
                     x = 0.855, 
                     y = 0.7, 
                     width = .2, 
                     height = .2)

LA_p1 <- 
  cowplot::plot_grid(
    circular_bar_h2[["Latin America"]],
    plotlist = m2_change_region[LatinA], 
    nrow = 1,
    rel_widths = c(2, 1, 1, 1, 1))

LA_h2 <-
  cowplot::ggdraw(LA_p1) +
  cowplot::draw_plot(LA_map_list$arid,
                     x = 0.365, 
                     y = 0.7, 
                     width = .2, 
                     height = .2) +
  cowplot::draw_plot(LA_map_list$polar, 
                     x = 0.535, 
                     y = 0.7, 
                     width = .2, 
                     height = .2) +
  cowplot::draw_plot(LA_map_list$temp, 
                     x = 0.7, 
                     y = 0.7, 
                     width = .2, 
                     height = .2) +
  cowplot::draw_plot(LA_map_list$tropical, 
                     x = 0.85, 
                     y = 0.7, 
                     width = .2, 
                     height = .2)

europe_p1 <- 
  cowplot::plot_grid(
    circular_bar_h2[["Europe"]],
    plotlist = m2_change_region[Europe], 
    nrow = 1,
    rel_widths = c(2, 1, 1, 1, 1))

europe_h2 <-
  cowplot::ggdraw(europe_p1) +
  cowplot::draw_plot(Europe_map_list$arid, 
                     x = 0.365, 
                     y = 0.7, 
                     width = .2, 
                     height = .2) +
  cowplot::draw_plot(Europe_map_list$cold, 
                     x = 0.535, 
                     y = 0.7, 
                     width = 0.2, 
                     height = .2) +
  cowplot::draw_plot(Europe_map_list$polar, 
                     x = 0.7, 
                     y = 0.7, 
                     width = .2, 
                     height = .2) +
  cowplot::draw_plot(Europe_map_list$temp, 
                     x = 0.855, 
                     y = 0.7, 
                     width = .2, 
                     height = .2)

asia_p1 <- 
  cowplot::plot_grid(
    circular_bar_h2[["Asia"]],
    plotlist = m2_change_region[Asia], 
    nrow = 1,
    rel_widths = c(2, 1, 1, 1, 1))

asia_h2 <-
  cowplot::ggdraw(asia_p1) +
  cowplot::draw_plot(Asia_map_list$arid, 
                     x = 0.365, 
                     y = 0.7, 
                     width = .2, 
                     height = .2) +
  cowplot::draw_plot(Asia_map_list$cold, 
                     x = 0.535, 
                     y = 0.7, 
                     width = .2, 
                     height = .2) +
  cowplot::draw_plot(Asia_map_list$polar, 
                     x = 0.7, 
                     y = 0.7, 
                     width = .2, 
                     height = .2) +
  cowplot::draw_plot(Asia_map_list$temp, 
                     x = 0.855, 
                     y = 0.7, 
                     width = .2, 
                     height = .2)

oceania_p1 <- 
  cowplot::plot_grid(
    circular_bar_h2[["Oceania"]],
    plotlist = m2_change_region[Oceania], 
    "NULL",
    "NULL",
    nrow = 1,
    rel_widths = c(2, 1, 1, 1, 1))

oceania_h2 <-
  cowplot::ggdraw(oceania_p1) +
  cowplot::draw_plot(oceania_map_list$temp, 
                     x = 0.7, 
                     y = 0.7, 
                     width = .2, 
                     height = .2) +
  cowplot::draw_plot(oceania_map_list$tropical, 
                     x = 0.855, 
                     y = 0.7, 
                     width = .2, 
                     height = .2)



# FINAL COMBINE ALL REGIONS
combine_h2 <-
  cowplot::plot_grid(NA_h2, 
                     LA_h2,
                     europe_h2,
                     asia_h2,
                     oceania_h2,
                     ncol = 1)

# SAVE

pdf("combined_h2_version2.pdf", width = 7, height = 7.5)  
combine_h2  
dev.off()

system2('open', args = 'combined_h2_version2.pdf')

ggsave(
  paste0("m2_change_time_,", select_region, ".png"),
  plot_m2,
  width = 8, height = 6, units = "cm",
  scaling = 0.5,
  bg = "white"
)

