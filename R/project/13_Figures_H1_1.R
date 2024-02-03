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

# - Load meta data
source(
  here::here(
    "R/project/02_meta_data.R"
  )
)

#----------------------------------------------------------#
# 1. Helper functions  -----
#----------------------------------------------------------#

# function get predictor barplot -----
get_predictor_barplot_for_all_regions <- function(
    data_source,
    sel_predictor, sel_palette,
    axis_to_right = TRUE) {
  data_bars <-
    data_source %>%
    tidyr::unnest(data_temporal) %>%
    tidyr::complete(
      age,
      tidyr::nesting(predictor, region)
    ) %>%
    dplyr::mutate(
      no_data = ifelse(is.na(percentage_median), TRUE, FALSE)
    ) %>%
    dplyr::mutate(
      percentage_median = ifelse(no_data, 0, percentage_median)
    ) %>%
    dplyr::group_by(region) %>%
    tidyr::nest(data_to_plot = -c(region)) %>%
    dplyr::mutate(
      plot = purrr::map(
        .x = data_to_plot,
        .f = ~ get_predictor_barplot(
          data = .x,
          sel_predictor = sel_predictor,
          x_var = "percentage_median",
          axis_to_right = axis_to_right,
          sel_palette = sel_palette
        )
      )
    )
  
  data_bars %>%
    purrr::chuck("plot") %>%
    rlang::set_names(
      nm = data_bars$region
    ) %>%
    return()
}


# function for plot dist density -----

plot_dist_density <- function(
    data_source, sel_predictor,
    text_size = 6,
    axis_to_right = TRUE) {
  data_work <-
    dplyr::filter(
      data_source, predictor == sel_predictor
    )
  
  fig <-
    data_work %>%
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = percentage
      )
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_colour_manual(
      values = palette_predictors_parts # [config criteria]
    ) +
    ggplot2::scale_fill_manual(
      values = palette_predictors_parts # [config criteria]
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      text = ggplot2::element_text(
        size = text_size
      ),
      line = ggplot2::element_line(
        linewidth = 0.01 # [config criteria]
      ),
      legend.position = "none",
      plot.margin = grid::unit(c(0, 0, 0, 0), "mm"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(
        fill = "transparent", color = NA
      ),
      plot.background = ggplot2::element_rect(
        fill = "transparent", color = NA
      ),
      panel.border = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.ticks.length = grid::unit(0, "mm")
    ) +
    ggplot2::labs(
      x = "Explained variability (%)",
      y = "Number of records"
    ) +
    ggplot2::geom_density(
      mapping = ggplot2::aes(
        y = after_stat(count),
        col = var_part,
        fill = var_part
      ),
      alpha = 0.4,
      linewidth = 0.1
    )
  
  if (isTRUE(axis_to_right)) {
    fig <-
      fig +
      ggplot2::scale_x_continuous(
        name = NULL,
        limits = c(0, 100),
        breaks = seq(0, 100, by = 25),
        expand = c(0, 0),
        position = "top"
      ) +
      ggplot2::scale_y_continuous(
        limits = c(0, NA)
      )
  } else {
    fig <-
      fig +
      ggplot2::scale_x_continuous(
        name = NULL,
        limits = c(0, 100),
        breaks = seq(0, 100, by = 25),
        expand = c(0, 0),
        position = "bottom"
      ) +
      ggplot2::scale_y_continuous(
        trans = "reverse",
        limits = c(NA, 0)
      )
  }
  
  return(fig)
}

# function add circle to map ----

add_circe_to_map <- function(
    sel_region,
    sel_circle_height = 80,
    sel_circle_width = 100,
    sel_bar_width = 55,
    sel_bar_height = 55,
    bar_offset = 30) {
  
  data_region_circle_position <-
    tibble::tribble(
      ~region, ~center_x, ~center_y,
      "North America", -150, 45,
      "Europe", 0, 45,
      "Asia", 150, 45,
      "Latin America", -60, -25,
      "Oceania", 120, -25
    )
  
  circle_width_diam <- sel_circle_width / 2
  circle_height_diam <- sel_circle_height / 2
  bar_height_diam <- sel_bar_height / 2
  
  circle_left <-
    data_region_circle_position %>%
    dplyr::filter(region == sel_region) %>%
    dplyr::pull(center_x) -
    circle_width_diam
  
  circe_right <- circle_left + sel_circle_width
  
  circle_top <-
    data_region_circle_position %>%
    dplyr::filter(region == sel_region) %>%
    dplyr::pull(center_y) +
    circle_height_diam
  
  circle_bottom <- circle_top - sel_circle_height
  
  bar_clim_left <- circle_left - bar_offset
  bar_clim_right <- bar_clim_left + sel_bar_width
  
  bar_hum_right <- circe_right + bar_offset
  bar_hum_left <- bar_hum_right - sel_bar_width
  
  bar_top <-
    data_region_circle_position %>%
    dplyr::filter(region == sel_region) %>%
    dplyr::pull(center_y) +
    bar_height_diam
  
  bar_bottom <- bar_top - sel_bar_height
  
  res <-
    list(
      ggplot2::annotation_custom(
        grob = ggplot2::ggplotGrob(
          cowplot::plot_grid(
            list_bars_plot_climate[[sel_region]],
            list_density_plots_climate[[sel_region]],
            rel_widths = c(0.8, 1)
          )
        ),
        xmin = bar_clim_left,
        xmax = bar_clim_right,
        ymin = bar_bottom,
        ymax = bar_top
      ),
      ggplot2::annotation_custom(
        grob = ggplot2::ggplotGrob(
          cowplot::plot_grid(
            list_density_plots_human[[sel_region]],
            list_bars_plot_human[[sel_region]],
            rel_widths = c(1, 0.8)
          )
        ),
        xmin = bar_hum_left,
        xmax = bar_hum_right,
        ymin = bar_bottom,
        ymax = bar_top
      ),
      ggplot2::annotation_custom(
        grob = ggplot2::ggplotGrob(
          list_circle_plots[[sel_region]]
        ),
        xmin = circle_left,
        xmax = circe_right,
        ymin = circle_bottom,
        ymax = circle_top
      )
    )
  
  return(res)
}

#----------------------------------------------------------#
# 1. Data Wrangling -----
#----------------------------------------------------------#
# Combine output tables of spatial and temporal data
input_spatial <-
  summary_spatial_median %>%
  dplyr::mutate(
    sel_classification = factor(sel_classification)
  ) %>%
  dplyr::mutate(
    predictor = factor(
      predictor,
      levels = predictors_spatial_order # [config criteria]
    )
  ) %>%
  dplyr::filter(n_records > 5) %>%
  tidyr::nest(data_spatial = -c(region))

input_temporal <-
  summary_temporal_median %>%
  dplyr::mutate(
    predictor = factor(predictor,
                       levels = c(
                         "human",
                         "climate"
                       )
    )
  ) %>%
  tidyr::nest(data_temporal = -c(region))

data_dist <-
  data_spatial_vis %>%
  dplyr::mutate(
    predictor = factor(
      predictor,
      levels = c("human", "climate", "time")
    )
  ) %>%
  dplyr::mutate(
    sel_classification = factor(sel_classification)
  ) %>%
  tidyr::pivot_longer(
    c(unique_percent, average_share_percent, individual_percent),
    names_to = "var_part",
    values_to = "percentage"
  ) %>%
  dplyr::mutate(
    var_part = factor(var_part,
                      levels = c(
                        "unique_percent",
                        "average_share_percent",
                        "individual_percent"
                      )
    )
  )

data_for_plotting <-
  dplyr::inner_join(
    input_spatial,
    input_temporal,
    by = "region"
  )

#----------------------------------------------------------#
# 2. Figures -----
#----------------------------------------------------------#

#----------------------------------------------------------#
# 2.1 Map -----
#----------------------------------------------------------#
# get basemap
world <- map_data("world")

# grey basemap
worldmap_grey <-
  world %>%
  dplyr::filter(lat > -60 & lat < 85) %>%
  ggplot2::ggplot() +
  ggplot2::geom_polygon(
    ggplot2::aes(
      x = long,
      y = lat,
      group = group
    ),
    fill = "grey80",
    alpha = 0.4
  ) +
  ggplot2::coord_equal(ratio = 1.3) +
  ggplot2::theme_void()

#----------------------------------------------------------#
# 2.2 Barplots -----
#----------------------------------------------------------#

# need to fix issue with dropping unused levels all should be from 500 - 8500

# barplot climate
list_bars_plot_climate <-
  get_predictor_barplot_for_all_regions(
    data_source = data_for_plotting,
    sel_predictor = "climate",
    sel_palette = palette_predictors,
    axis_to_right = FALSE
  )


# barplot human
list_bars_plot_human <-
  get_predictor_barplot_for_all_regions(
    data_source = data_for_plotting,
    sel_predictor = "human",
    sel_palette = palette_predictors,
    axis_to_right = TRUE
  )

#----------------------------------------------------------#
# 2.3 Circular barplots -----
#----------------------------------------------------------#

data_circle_plots <-
  data_for_plotting %>%
  tidyr::unnest(data_spatial) %>%
  dplyr::group_by(region) %>%
  tidyr::nest(data_to_plot = -c(region)) %>%
  dplyr::mutate(
    plot = purrr::map(
      .x = data_to_plot,
      .f = ~ get_circular_barplot(
        data = .x,
        y_var = "percentage_median",
        x_var = "predictor",
        col_vec = palette_ecozones, # [config criteria]
        x_name = predictors_label, # [config criteria]
        icon_size = 0.15,
        y_max = 45
      )
    )
  )

list_circle_plots <-
  data_circle_plots %>%
  purrr::chuck("plot") %>%
  rlang::set_names(
    nm = unique(data_circle_plots$region)
  )

#----------------------------------------------------------#
# 2.4 Density plots -----
#----------------------------------------------------------#

# - Density figures for full distribution of variance
# - Split by human and climate
data_density_plot <-
  data_dist %>%
  dplyr::group_by(region) %>%
  tidyr::nest(data_dist = -c(region)) %>%
  dplyr::mutate(
    plot_human = purrr::map(
      .x = data_dist,
      .f = ~ plot_dist_density(
        data_source = .x,
        sel_predictor = "human",
        axis_to_right = FALSE
      )
    ),
    plot_climate = purrr::map(
      .x = data_dist,
      .f = ~ plot_dist_density(
        data_source = .x,
        sel_predictor = "climate",
        axis_to_right = TRUE
      )
    )
  )

list_density_plots_human <-
  data_density_plot %>%
  purrr::chuck("plot_human") %>%
  rlang::set_names(
    nm = unique(data_density_plot$region)
  )

list_density_plots_climate <-
  data_density_plot %>%
  purrr::chuck("plot_climate") %>%
  rlang::set_names(
    nm = unique(data_density_plot$region)
  )

#----------------------------------------------------------#
# 2.5 Final map -----
#----------------------------------------------------------#

# - Inset figures on map
combined_map_h1 <-
  worldmap_grey +
  add_circe_to_map("North America") +
  add_circe_to_map("Europe") +
  add_circe_to_map("Asia") +
  add_circe_to_map("Latin America") +
  add_circe_to_map("Oceania")

combined_map_h1_with_space_for_legend <-
  cowplot::plot_grid(
    combined_map_h1,
    patchwork::plot_spacer(),
    nrow = 2,
    rel_heights = c(3, 1)
  )

#----------------------------------------------------------#
# 3 Save final figure -----
#----------------------------------------------------------#

# save -----
purrr::walk(
  .x = c("png", "pdf"),
  .f = ~ ggplot2::ggsave(
    paste(
      here::here("Outputs/combined_map_h1"),
      .x,
      sep = "."
    ),
    plot = combined_map_h1_with_space_for_legend,
    width = image_width_vec["2col"],
    height = 100, # 85
    units = "mm",
    bg = "white"
  )
)

# end script -----


