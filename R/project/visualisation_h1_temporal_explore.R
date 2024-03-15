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

# Predictors
palette_predictors <- c(
  human = "grey50", # v#ffa600"
  climate = "grey30" # v#d74e92"
)


# predictor parts
palette_human_pred <-
  c(
    "#9C8A6C",
    "grey35"
  ) %>%
  rlang::set_names(
    nm = c(
      "spd",
      "events"
    )
  )
#----------------------------------------------------------#
# 1. Summary tables -----
#----------------------------------------------------------#

data_spd_temporal_summary_by_region <-
  output_temporal_spd %>%
  get_summary_tables(
    data_source = .,
    data_type = "temporal",
    group_var = c("region")
  )
  

data_events_temporal_summary_by_region <-
  output_temporal_events %>%
  get_summary_tables(
    data_source = .,
    data_type = "temporal",
    group_var = c("region")
  )
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



main_temporal_fig <-
  data_combined_temporal %>%
  ggplot2::ggplot() +
  ggplot2::geom_rect(
    data = ages_for_shading,
    ggplot2::aes(
      xmin = as.factor(start_age/1000),
      xmax = as.factor(end_age/1000),
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "grey50",
    alpha = 0.2
  ) +
  ggplot2::geom_bar(
    data = . %>%
      dplyr::filter(
        importance_type == "ratio_ind_wmean"
      ),
    ggplot2::aes(
      x = as.factor(age / 1000),
      y = get("ratio"),
      fill = human_pred
    ),
    stat = "identity",
    width = 0.9,
    alpha = 1,
    position = ggplot2::position_dodge2(
      width = 0.9,
      preserve = "single"
    ),
    show.legend = TRUE
  ) +
  geom_smooth(aes(
    x = as.factor(age / 1000),
    y = get("ratio"),
    col = human_pred,
    group = human_pred),
    se = FALSE,
    linetype = "dashed",
    alpha = 0.5,
    linewidth = 0.1,
    show.legend = FALSE) +
  ggplot2::scale_y_continuous(
    limits = c(-0.2, 1.2),
    breaks = seq(-0.2, 1.2, 0.2),
    oob = scales::squish
  ) +
  ggplot2::scale_x_discrete(limit = rev) +
  scale_fill_manual(
    values = palette_human_pred,
    drop = FALSE) + 
  scale_colour_manual(
    values = palette_human_pred,
    drop = FALSE) +
  guides(colour = "none")+
  ggplot2::theme(
    aspect.ratio = 1/2,
    legend.position = "top",
   # panel.background = ggplot2::element_blank(),
    strip.background.y = ggplot2::element_blank(),
    #strip.text.y = ggplot2::element_blank(),
    legend.title = element_text(size=10),
    #panel.grid.minor = ggplot2::element_blank(),
    plot.background = ggplot2::element_rect(
      fill = "transparent",
      color = NA
    ),
    panel.grid.major = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_text(size = 8),
    axis.title.y = ggplot2::element_text(size = 8),
    axis.text.x = ggplot2::element_text(
      size = 8, angle = 60),
    axis.text.y = ggplot2::element_text(size = 8),
   # plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm")
  ) +
  ggplot2::facet_grid(predictor ~ region) +
  ggplot2::labs(x = "Age (ka) BP", 
                y = "Ratio of importance",
                fill = "Analyses run with human predictor") 

main_temporal_fig

#----------------------------------------------------------#
# 4. save figure -----
#----------------------------------------------------------#



purrr::walk(
  .x = c("png", "pdf"),
  .f = ~ ggplot2::ggsave(
    paste(
      here::here("Outputs/temporal_h1"),
      .x,
      sep = "."
    ),
    plot = main_temporal_fig,
    width = image_width_vec["3col"], # [config criteria]
    height = 80,
    units = image_units, # [config criteria]
    bg = "white"
  )
)










