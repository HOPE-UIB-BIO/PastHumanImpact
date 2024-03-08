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

# - Load meta data
source(
  here::here(
    "R/project/02_meta_data.R"
  )
)

# - Load list of summary tables from pipeline spd
output_spatial_spd <-
  targets::tar_read(
    name = "output_spatial_spd",
    store = paste0(
      data_storage_path,
      "_targets_data/analyses_h1"
    )
  )


output_temporal_spd <-
  targets::tar_read(
    name = "output_temporal_spd",
    store = paste0(
      data_storage_path,
      "_targets_data/analyses_h1"
    )
  )

# - Load list of summary tables from events
output_spatial_events <-
  targets::tar_read(
    name = "output_spatial_events",
    store = paste0(
      data_storage_path,
      "_targets_data/analyses_h1"
    )
  )

output_temporal_events <-
  targets::tar_read(
    name = "output_temporal_events",
    store = paste0(
      data_storage_path,
      "_targets_data/analyses_h1"
    )
  )


# ------------------------------------ #

data_spd_spatial_summary_by_climatezone <-
  output_spatial_spd %>%
  dplyr::left_join(
    data_meta %>%
      dplyr::select(
        dataset_id, region, climatezone
      ),
    by = "dataset_id"
  ) %>%
  get_summary_tables(
    data_source = .,
    data_type = "spatial",
    group_var = c("region", "climatezone")
  )

data_spd_temporal <-
  output_temporal_spd %>%
  get_summary_tables(
    data_source = .,
    data_type = "temporal",
    group_var = c("region")
  )

data_events_spatial_summary_by_climatezone <-
  output_spatial_events %>%
  dplyr::left_join(
    data_meta %>%
      dplyr::select(
        dataset_id, region, climatezone
      ),
    by = "dataset_id"
  ) %>%
  get_summary_tables(
    data_source = .,
    data_type = "spatial",
    group_var = c("region", "climatezone")
  )

data_events_temporal <-
  output_temporal_events %>%
  get_summary_tables(
    data_source = .,
    data_type = "temporal",
    group_var = c("region")
  )


#----------------------------------------------------------#
# 2. Combine output data -----
#----------------------------------------------------------#
# Combine summary output for plotting ----
data_for_plotting <-
  dplyr::inner_join(
    data_spd_spatial_summary_by_climatezone %>%
      purrr::chuck("summary_table_weighted_mean") %>%
      tidyr::nest(data_spatial = -c(region)),
    data_spd_temporal %>%
      purrr::chuck("summary_table_weighted_mean") %>%
      tidyr::nest(data_spatial = -c(region)),
    by = "region"
  ) %>%
  dplyr::left_join(
    data_events_temporal %>%
      purrr::chuck("summary_table_weighted_mean") %>%
      tidyr::nest(data_spatial = -c(region)),
    by = "region"
  ) %>%
  rlang::set_names(c("region", "spd_spatial", "spd_temporal", "events_temporal"))

# Combine temporal spd and events data for plotting ----
data_source_temporal <-
  data_for_plotting %>%
  dplyr::select(region, spd_temporal) %>%
  tidyr::unnest(col = spd_temporal) %>%
  dplyr::mutate(human_pred = "spd") %>%
  dplyr::full_join(
    data_for_plotting %>%
      dplyr::select(region, events_temporal) %>%
      tidyr::unnest(col = events_temporal) %>%
      dplyr::mutate(human_pred = "events"),
    by = dplyr::join_by(
      region,
      age, predictor,
      importance_type, ratio,
      human_pred
    )
  ) %>%
  dplyr::mutate(
    predictor = factor(predictor, levels = c("human", "climate"))
  )

# Distribution of ratios for individual records ----
data_dist <-
  data_spd_spatial_summary_by_climatezone %>%
  purrr::chuck("summary_table") %>%
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
      levels = c(
        "human",
        "climate"
      )
    )
  ) %>%
  dplyr::mutate(
    climatezone = factor(climatezone)
  ) %>%
  tidyr::pivot_longer(
    c(
      ratio_unique,
      ratio_ind
    ),
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
# temporal plot ----

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
    ggplot2::scale_y_continuous(
      limits = c(-0.2, 1.5),
      breaks = seq(-0.2, 1, 0.2)
    ) +
    ggplot2::scale_x_discrete(limit = rev) +
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
    ) +
    ggplot2::facet_wrap(~predictor, ncol = 1) +
    ggplot2::labs(x = "Age (ka) BP", y = "Ratio of importance")

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

data_dist %>%
  # filter(n_records > 5) %>%
  ggplot2::ggplot(
    mapping = ggplot2::aes(
      x = predictor,
      y = ratio
    )
  ) +
  ggdist::stat_halfeye(
    mapping = ggplot2::aes(
      color = predictor,
      fill = after_scale(lighten(color, .1))
    ),
    # adjust = .5,
    width = .5,
    .width = 0,
    justification = -.7,
    point_colour = NA
  ) +
  ggplot2::geom_boxplot(
    mapping = ggplot2::aes(
      color = climatezone,
      color = ggplot2::after_scale(
        colorspace::darken(color, .1)
      ),
      fill = ggplot2::after_scale(
        desaturate(
          colorspace::lighten(color, .8), .4
        )
      )
    ),
    width = .6,
    outlier.shape = NA
  ) +
  ggplot2::geom_point(
    aes(
      color = climatezone,
      color = after_scale(darken(color, .1))
    ),
    fill = "white",
    shape = 21,
    stroke = .4,
    size = 2,
    position = position_dodge(width = .6)
  ) +
  ggplot2::geom_point(
    mapping = ggplot2::aes(fill = climatezone),
    color = "transparent",
    shape = 21,
    stroke = .4,
    size = 2,
    alpha = .3,
    position = position_dodge(width = .6)
  ) +
  ggplot2::theme(
    legend.position = "none"
  ) +
  ggplot2::scale_y_continuous(
    limits = c(-.2, 1.5),
    expand = c(0, 0),
    breaks = seq(0, 1.5, by = .2)
  ) +
  ggplot2::scale_fill_manual(values = palette_ecozones) +
  ggplot2::scale_color_manual(values = palette_ecozones) +
  ggplot2::coord_flip() +
  ggplot2::labs(x = "", y = "%") +
  ggplot2::facet_wrap(~region, nrow = 1)
