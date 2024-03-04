#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                      Visualisation
#                      FIGURE 1 H1
#
#                   O. Mottl, V.A. Felde
#                         2023
#
#----------------------------------------------------------#


#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

library(here)

# - Load configuration
source(
  here::here(
    "R/project/00_Config_file.R"
  )
)

#----------------------------------------------------------#
# 1. Load results -----
#----------------------------------------------------------#

# - Load list of summary tables from pipeline spd
summary_tables_spd <- targets::tar_read(
  name = "summary_tables_spd",
  store = paste0(
    data_storage_path,
    "_targets_data/analyses_h1"
  )
)

# - Load list of summary tables from events
summary_tables_events <- targets::tar_read(
  name = "summary_tables_events",
  store = paste0(
    data_storage_path,
    "_targets_data/analyses_h1"
  )
)

#----------------------------------------------------------#
# 2. Combine output data -----
#----------------------------------------------------------#
# Combine summary output for plotting ----
data_for_plotting <-
  dplyr::inner_join(
    summary_tables_spd$summary_spatial_long %>%
      tidyr::nest(data_spatial = -c(region)),
    summary_tables_spd$summary_temporal_long %>%
      tidyr::nest(data_temporal_spd = -c(region)),
    by = "region"
  ) %>%
  left_join(
    summary_tables_events$summary_temporal_long %>%
      tidyr::nest(data_temporal_events = -c(region)),
    by = "region"
  ) 

# Combine temporal spd and events data for plotting ----
data_source_temporal <- data_for_plotting %>%
  dplyr::select(region, data_temporal_spd) %>%
  unnest(col=data_temporal_spd) %>%
  mutate(human_pred = "spd") %>%
  full_join(
    data_for_plotting %>%
      dplyr::select(region, data_temporal_events) %>%
      unnest(col=data_temporal_events) %>%
      mutate(human_pred = "events")) %>%
  mutate(predictor = factor(predictor, levels = c("human", "climate"))) 

# Distribution of ratios for individual records ----
data_dist <- 
  summary_tables_spd$summary_table_spatial %>%
  dplyr::select(
    dataset_id,
    region, 
    climatezone,
    predictor,
    ratio_unique,
    ratio_ind,
    n_records
  ) %>%
  dplyr::mutate(
    predictor = factor(
      predictor,
      levels = c("human", 
                 "climate")
      )
  ) %>%
  dplyr::mutate(
    climatezone = factor(climatezone)
  ) %>%
  tidyr::pivot_longer(
    c(ratio_unique, 
      ratio_ind),
    names_to = "importance_type",
    values_to = "ratio"
  ) %>%
  dplyr::mutate(
    importance_type = factor(importance_type,
                             levels = c(
                               "ratio_unique",
                               "ratio_ind"
                               )
                             )
    ) 



#----------------------------------------------------------#
# 2. helper functions for plotting -----
#----------------------------------------------------------#
#temporal plot ----

get_figure_temporal <- function(data_source_temporal) {
  
  figure_temporal <-
    data_source_temporal %>% 
    ggplot2::ggplot() +
    ggplot2::geom_bar(
      data = data_source_temporal %>%
        dplyr::filter(
          importance_type == "ratio_unique_wmean"
        ),
      mapping = ggplot2::aes(
        x = as.factor(age / 1000),
        y = get("ratio"),
        fill = human_pred
      ),
      stat = "identity",
      width = .6,
      alpha = 1,
      position = ggplot2::position_dodge2(
        width = 0.8,
        preserve = "single"
      ),
      show.legend = FALSE
    ) +
    ggplot2::geom_bar(
      data = data_source_temporal %>%
        dplyr::filter(
          importance_type == "ratio_ind_wmean"
        ),
      ggplot2::aes(
        x = as.factor(age / 1000),
        y = get("ratio"),
        fill = human_pred
      ),
      stat = "identity",
      width = .6,
      alpha = 0.4,
      position = ggplot2::position_dodge2(
        width = 0.8,
        preserve = "single"
      ),
      show.legend = FALSE
    ) +
    scale_y_continuous(
      limits = c(-0.2, 1.5),
      breaks = seq(-0.2, 1, 0.2)
    ) +
    scale_x_discrete(limit = rev) +
    
    ggplot2::theme(
      panel.background = ggplot2::element_rect(
        fill = "transparent", color = NA
      ),
      plot.background = ggplot2::element_rect(
        fill = "transparent", color = NA
      ),
      line = ggplot2::element_line(linewidth = 0.01),
      text = ggplot2::element_text(size = 15, color = "grey30"),
      # plot.margin = grid::unit(c(0, 0, 20, 20), "mm"),
      # axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(hjust = 1),
      axis.ticks.x = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(size = 12, color = "grey30"),
      axis.line = ggplot2::element_blank()
    )+
    facet_wrap(~ predictor, ncol = 1) +
    labs(x = "Age (ka) BP", y = "Ratio of importance") 
  
  return(figure_temporal)
  
}

# spatial plot ----
get_spatial_barplot <- function(data_source_spatial) {
  
  figure_spatial <- 
  data_source_spatial %>%
  ggplot2::ggplot(
    mapping = ggplot2::aes(
      x = get("ratio"),
      y = get("predictor"),
      fill = get("climatezone")
    )
  ) +
  ggplot2::theme(
    legend.position = "none",
    panel.background = ggplot2::element_rect(
      fill = "white",
      color = NA
    ),
    plot.background = ggplot2::element_rect(
      fill = "white",
      color = NA
    ),
    panel.border = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    plot.margin = grid::unit(c(0, 0, 0, 0), "mm"),
    panel.spacing = grid::unit(c(0, 0, 0, 0), "mm"),
    text = ggplot2::element_text(size = 15, color = "grey30")
  ) +
  ggplot2::scale_fill_manual(
    values = palette_ecozones,
    drop = FALSE
  ) +
  
  ggplot2::geom_col(
    data = data_source_spatial %>%
      dplyr::filter(
        grepl(
          "ratio_unique_wmean",
          importance_type
        )
      ),
    width = 0.6,
    position = ggplot2::position_dodge2(
      width = 0.8,
      preserve = "single"
    ),
    alpha = 1
  ) +
  ggplot2::geom_col(
    data = data_source_spatial %>%
      dplyr::filter(grepl(
        "ratio_ind_wmean",
        importance_type
      )),
    width = .6,
    position = ggplot2::position_dodge2(
      width = 0.8,
      preserve = "single"
    ),
    alpha = 0.4
  )  
return(figure_spatial)
}