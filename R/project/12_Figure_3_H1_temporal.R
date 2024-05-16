#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                      Hypothesis I
#
#
#                   O. Mottl, V.A. Felde
#                         2023
#
#----------------------------------------------------------#

#----------------------------------------------------------#
# 0. Load data -----
#----------------------------------------------------------#
# Load configuration
source(
  here::here(
    "R/project/00_Config_file.R"
  )
)

# - Load meta data
source(
  here::here(
    "R/project/02_meta_data.R"
  )
)

# Load temporal spd
output_temporal_spd <-
  targets::tar_read(
    name = "output_temporal_spd",
    store = paste0(
      data_storage_path,
      "_targets_data/analyses_h1"
    )
  )

# Load temporal events
output_temporal_events <-
  targets::tar_read(
    name = "output_temporal_events",
    store = paste0(
      data_storage_path,
      "_targets_data/analyses_h1"
    )
  )


#----------------------------------------------------------#
# 1. Summary tables -----
#----------------------------------------------------------#
#spd
data_spd_temporal_summary_by_region <-
  output_temporal_spd %>%
  get_summary_tables(
    data_source = .,
    data_type = "temporal",
    group_var = c("region")
  )

# save table output
data_spd_temporal_summary_by_region %>%
  purrr::chuck("summary_table_weighted_mean") %>%
  dplyr::filter(importance_type == "ratio_ind_wmean") %>%
  pivot_wider(
    names_from = region,
    values_from = ratio
  ) %>%
  dplyr::select(
    -importance_type) %>%
  write_csv(here::here("summary_temporal_spd.csv"))  

data_spd_temporal_summary_by_region %>%
  purrr::chuck("summary_table_weighted_mean") %>%
  dplyr::filter(importance_type == "ratio_ind_wmean") %>%
  group_by(age, predictor) %>%
  summarise(
    min = min(ratio),
    med = median(ratio),
    max = max(ratio)
  ) %>%
  write_csv(here::here("summary_temporal_spd_by_age.csv"))

# events
data_events_temporal_summary_by_region <-
  output_temporal_events %>%
  get_summary_tables(
    data_source = .,
    data_type = "temporal",
    group_var = c("region")
  )


# save table output
data_events_temporal_summary_by_region %>%
  purrr::chuck("summary_table_weighted_mean") %>%
  dplyr::filter(importance_type == "ratio_ind_wmean") %>%
  pivot_wider(
    names_from = region,
    values_from = ratio
  ) %>%
  dplyr::select(
    -importance_type) %>%
  write_csv(here::here("summary_temporal_events.csv"))

data_events_temporal_summary_by_region %>%
  purrr::chuck("summary_table_weighted_mean") %>%
  dplyr::filter(importance_type == "ratio_ind_wmean") %>%
  group_by(age, predictor) %>%
  summarise(
    min = min(ratio),
    med = median(ratio),
    max = max(ratio)
  ) %>%
  write_csv(here::here("summary_temporal_events_by_age.csv"))


#----------------------------------------------------------#
# 2. helper functions and data wrangling -----
#----------------------------------------------------------#

add_predictor_as_factor <- function(data_source) {
  data_source %>%
    dplyr::mutate(
      predictor = factor(
        predictor,
        levels = c("human", "climate")
      )
    ) %>%
    return()
}

add_region_as_factor <- function(data_source) {
  data_source %>%
    dplyr::mutate(
      region = factor(
        region,
        levels = vec_regions # [config criteria]
      )
    ) %>%
    return()
}

# order factors by predictor and region
data_spd_region <-
  data_spd_temporal_summary_by_region %>%
  purrr::chuck("summary_table_weighted_mean") %>%
  filter(age > 1900) %>%
  add_predictor_as_factor() %>%
  add_region_as_factor() %>%
  mutate(human_pred = "spd")


data_events_region <-
  data_events_temporal_summary_by_region %>%
  purrr::chuck("summary_table_weighted_mean") %>%
  add_predictor_as_factor() %>%
  add_region_as_factor() %>%
  mutate(human_pred = "events")

data_combined_temporal <- 
  data_spd_region %>%
  dplyr::bind_rows(data_events_region)

ages_for_shading <-
  tibble(region = c( "North America",
                     "North America",
                     "Latin America",
                     "Latin America", 
                     "Latin America",
                     "Europe", 
                     "Europe",
                     "Europe",
                     "Asia", 
                     "Oceania"),
             start_age = c(8500,
                           1000, 
                           8000,
                           5000,
                           1500,
                           8000,
                           6500,
                           4000,
                           5000, 
                           3500),
             end_age = c( 8000,
                          0, 
                          7500,
                          2500, 
                          0,
                          7000,
                          6000,
        
                          0,
                          0, 
                          0)
             ) %>%
  add_region_as_factor()
#----------------------------------------------------------#
# 3. build figure -----
#----------------------------------------------------------#



# main_temporal_fig <-
#   data_combined_temporal %>%
#   ggplot2::ggplot() +
#   # ggplot2::geom_rect(
#   #   data = ages_for_shading,
#   #   ggplot2::aes(
#   #     xmin = as.factor(start_age/1000),
#   #     xmax = as.factor(end_age/1000),
#   #     ymin = -Inf,
#   #     ymax = Inf
#   #   ),
#   #   fill = "grey50",
#   #   alpha = 0.2
#   # ) +
#   ggplot2::geom_bar(
#     data = . %>%
#       dplyr::filter(
#         importance_type == "ratio_ind_wmean"
#       ),
#     ggplot2::aes(
#       x = as.factor(age / 1000),
#       y = get("ratio"),
#       fill = human_pred
#     ),
#     stat = "identity",
#     width = 0.9,
#     alpha = 1,
#     position = ggplot2::position_dodge2(
#       width = 1,
#       preserve = "single"
#     ),
#     show.legend = TRUE
#   ) +
#   # geom_smooth(aes(
#   #   x = as.factor(age / 1000),
#   #   y = get("ratio"),
#   #   col = human_pred,
#   #   group = human_pred),
#   #   se = FALSE,
#   #   linetype = "dashed",
#   #   alpha = 0.5,
#   #   linewidth = 0.1,
#   #   show.legend = FALSE) +
#   ggplot2::scale_y_continuous(
#     limits = c(-0.2, 1.2),
#     breaks = seq(-0.2, 1.2, 0.2),
#     oob = scales::squish
#   ) +
#   ggplot2::scale_x_discrete(limit = rev) +
#   scale_fill_manual(
#     values = palette_human_pred,
#     drop = FALSE) + 
#   scale_colour_manual(
#     values = palette_human_pred,
#     drop = FALSE) +
#   guides(colour = "none")+
#   ggplot2::theme(
#     #aspect.ratio = 1/2,
#     legend.position = "top",
#     panel.background = ggplot2::element_blank(),
#     strip.background.y = ggplot2::element_blank(),
#     #strip.text.y = ggplot2::element_blank(),
#     legend.title = element_text(size=10),
#     #panel.grid.minor = ggplot2::element_blank(),
#     plot.background = ggplot2::element_rect(
#       fill = "transparent",
#       color = NA
#     ),
#     panel.grid.major = ggplot2::element_blank(),
#     axis.title.x = ggplot2::element_text(size = 8),
#     axis.title.y = ggplot2::element_text(size = 8),
#     axis.text.x = ggplot2::element_text(
#       size = 8, angle = 60),
#     axis.text.y = ggplot2::element_text(size = 8),
#    # plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm")
#   ) +
#   ggplot2::facet_grid(region ~ predictor) +
#   ggplot2::labs(x = "Age (ka) BP", 
#                 y = "Ratio of importance",
#                 fill = "HVARPART with human predictor") 
# 
# main_temporal_fig

main_temporal_fig2 <-
  data_combined_temporal %>%
  ggplot2::ggplot() +
  ggplot2::geom_bar(
    data = . %>%
      dplyr::filter(
        importance_type == "ratio_ind_wmean"
      ),
    ggplot2::aes(
      x = as.factor(age / 1000),
      y = get("ratio"),
      fill = predictor
    ),
    stat = "identity",
    width = 0.9,
    alpha = 1,
    position = "stack",
    show.legend = TRUE
  ) +
  ggplot2::scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    oob = scales::squish
  ) +
  ggplot2::scale_x_discrete(limit = rev) +
  scale_fill_manual(
    values = palette_predictors,
    drop = FALSE) + 
  scale_colour_manual(
    values = palette_predictors,
    drop = FALSE) +
  guides(colour = "none")+
  ggplot2::theme(
    legend.position = "top",
    panel.background = ggplot2::element_blank(),
    strip.background.y = ggplot2::element_blank(),
    legend.title = element_text(size=10),
    plot.background = ggplot2::element_rect(
      fill = "transparent",
      color = NA
    ),
    panel.grid.major = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_text(size = 10),
    axis.title.y = ggplot2::element_text(size = 8),
    axis.text.x = ggplot2::element_text(
      size = 10),
    axis.text.y = ggplot2::element_text(size = 8)
  ) +
  ggplot2::facet_grid(region ~ human_pred) +
  ggplot2::labs(x = "Age ka BP", 
                y = "Ratio of importance",
                fill = "Predictors") 

#----------------------------------------------------------#
# 4. save figure -----
#----------------------------------------------------------#



purrr::walk(
  .x = c("png", "pdf"),
  .f = ~ ggplot2::ggsave(
    paste(
      here::here("Outputs/Figure3_h1_temporal"),
      .x,
      sep = "."
    ),
    plot = main_temporal_fig2,
    width = image_width_vec["2col"], # [config criteria]
    height = 165,
    units = image_units, # [config criteria]
    bg = "white"
  )
)










