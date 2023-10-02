#----------------------------------------------------------#
#
#
#                   GlobalHumanImpact
#
#                    VISUALISATION
#    Supplementary: Distribution of H1 unique human impact
#
#                   V. Felde, O. Mottl
#                         2023
#
#----------------------------------------------------------#


#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

library(here)

# Load configuration
source(
  here::here(
    "R/00_Config_file.R"
  )
)

#----------------------------------------------------------#
# 1. Load data -----
#----------------------------------------------------------#

# Import tables for plotting
source(
  here::here(
    "R/working_scripts/Results_script.R"
  )
)


#----------------------------------------------------------#
# 2. Plot individual figures -----
#----------------------------------------------------------#

fig_unique_human_dist <-
  data_spatial_vis %>%
  dplyr::mutate(
    sel_classification = as.factor(sel_classification),
    region = factor(region,
      levels = vec_regions
    )
  ) %>%
  dplyr::full_join(
    data_climate_zones, # [config criteria]
    .,
    by = "sel_classification"
  ) %>%
  dplyr::filter(
    predictor == "human"
  ) %>%
  ggplot2::ggplot(
    mapping = ggplot2::aes(
      x = sel_classification,
      y = unique_percent
    )
  ) +
  ggplot2::facet_wrap(~region, ncol = 1) +
  ggplot2::scale_y_continuous(
    limits = c(0, 100)
  ) +
  ggplot2::scale_fill_manual(
    values = palette_ecozones # [config criteria]
  ) +
  ggplot2::scale_color_manual(
    values = palette_ecozones # [config criteria]
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    text = ggplot2::element_text(
      size = text_size # [config criteria]
    ),
    line = ggplot2::element_line(
      linewidth = line_size # [config criteria]
    ),
    legend.position = "none",
    axis.text.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    panel.spacing.x = grid::unit(0, "mm"),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    plot.margin = grid::unit(c(0.1, 0.1, 0.1, 0.1), "mm")
  ) +
  ggplot2::labs(
    x = "Climate zone",
    y = "Unique variance explained by human impact (%)"
  ) +
  ggplot2::geom_jitter(
    mapping = ggplot2::aes(
      col = sel_classification
    ),
    alpha = 0.3
  ) +
  ggplot2::geom_violin(
    mapping = ggplot2::aes(
      fill = sel_classification
    ),
    col = NA
  ) +
  ggplot2::geom_boxplot(
    fill = "white",
    col = "grey30",
    width = 0.1,
    outlier.shape = NA
  ) +
  ggplot2::geom_point(
    data = summary_spatial_median %>%
      dplyr::filter(
        predictor == "human" &
          variance_partition == "unique_percent_median"
      ) %>%
      dplyr::mutate(
        region = factor(region,
          levels = vec_regions
        )
      ),
    mapping = ggplot2::aes(
      x = sel_classification,
      y = percentage_median,
      fill = sel_classification
    ),
    shape = 22,
    col = "gray30",
    size = 3
  )


ggplot2::ggsave(
  paste0(
    here::here("Outputs/Supp"),
    "/unique_human_dist.png"
  ),
  plot = fig_unique_human_dist,
  width = image_width_vec["1col"], # [config criteria]
  height = 120,
  units = image_units, # [config criteria]
  bg = "white"
)
