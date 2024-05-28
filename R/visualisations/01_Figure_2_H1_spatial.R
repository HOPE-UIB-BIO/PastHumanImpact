#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                      Visualisation
#                      FIGURE 1 H1
#
#                   O. Mottl, V.A. Felde
#                         2024
#
#----------------------------------------------------------#


#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

library(here)

# - Load configuration
source(
  here::here(
    "R/00_Config_file.R"
  )
)

#----------------------------------------------------------#
# 1. Load results -----
#----------------------------------------------------------#

# - Load meta data
source(
  here::here(
    "R/main_analysis/02_meta_data.R"
  )
)

# - Load list of summary tables from pipeline spd
output_spatial_spd <-
  targets::tar_read(
    name = "output_spatial_spd",
    store = paste0(
      data_storage_path,
      "Targets_data/analyses_h1"
    )
  )
# - Load data geo koppen
data_geo_koppen <-
  readr::read_rds(
    paste0(
      data_storage_path,
      "Spatial/Climatezones/data_geo_koppen.rds"
    )
  ) %>%
  tibble::as_tibble() %>%
  dplyr::mutate(
    climatezone = dplyr::case_when(
      ecozone_koppen_15 == "Cold_Without_dry_season" ~ ecozone_koppen_30,
      ecozone_koppen_5 == "Cold" ~ ecozone_koppen_15,
      ecozone_koppen_5 == "Temperate" ~ ecozone_koppen_15,
      .default = ecozone_koppen_5
    )
  ) %>%
  add_climatezone_as_factor()

#----------------------------------------------------------#
# 2. Estimate averages -----
#----------------------------------------------------------#


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

data_spd_records <-
  data_spd_spatial_summary_by_climatezone %>%
  purrr::chuck("summary_table") %>%
  add_predictor_as_factor() %>%
  add_region_as_factor() %>%
  add_climatezone_as_factor()

data_spd_climatezone <-
  data_spd_spatial_summary_by_climatezone %>%
  purrr::chuck("summary_table_weighted_mean") %>%
  add_predictor_as_factor() %>%
  add_region_as_factor() %>%
  add_climatezone_as_factor()

data_spd_region <-
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
    group_var = c("region")
  ) %>%
  purrr::chuck("summary_table_weighted_mean") %>%
  add_predictor_as_factor() %>%
  add_region_as_factor()


#----------------------------------------------------------#
# 3. Records: estimate quantiles -----
#----------------------------------------------------------#

data_spd_records_quantiles <-
  data_spd_records %>%
  dplyr::group_by(
    region, climatezone, predictor
  ) %>%
  dplyr::summarise(
    .groups = "drop",
    q_95_upr = stats::quantile(ratio_ind, 0.975, na.rm = TRUE),
    q_95_lwr = stats::quantile(ratio_ind, 0.025, na.rm = TRUE),
    q_75_upr = stats::quantile(ratio_ind, 0.875, na.rm = TRUE),
    q_75_lwr = stats::quantile(ratio_ind, 0.125, na.rm = TRUE),
    q_50_upr = stats::quantile(ratio_ind, 0.75, na.rm = TRUE),
    q_50_lwr = stats::quantile(ratio_ind, 0.25, na.rm = TRUE)
  ) %>%
  tidyr::pivot_longer(
    cols = starts_with("q_"),
    names_to = "quantile",
    values_to = "value"
  ) %>%
  dplyr::mutate(
    quantile_type = stringr::str_sub(quantile, 6, 8),
    quantile_degree = stringr::str_sub(quantile, 3, 4)
  ) %>%
  dplyr::select(-quantile) %>%
  tidyr::pivot_wider(
    names_from = quantile_type,
    values_from = value
  ) %>%
  add_climatezone_as_factor()


#----------------------------------------------------------#
# 4. Build figure -----
#----------------------------------------------------------#

sel_range <- c(0, 1)

palette_ecozones_labels <-
  palette_ecozones %>%
  rlang::set_names(
    nm = get_climatezone_label(names(palette_ecozones))
  )

p0 <-
  tibble::tibble() %>%
  ggplot2::ggplot() +
  ggplot2::coord_cartesian(
    ylim = sel_range
  ) +
  ggplot2::theme(
    legend.position = "none",
    legend.text = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = common_gray # [config criteria]
    ),
    legend.title = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = common_gray # [config criteria]
    ),
    text = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = common_gray # [config criteria]
    ),
    axis.text.y = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = common_gray # [config criteria]
    ),
    axis.title.y = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = common_gray # [config criteria]
    ),
    line = ggplot2::element_line(
      linewidth = line_size, # [config criteria]
      color = common_gray # [config criteria]
    )
  ) +
  ggplot2::scale_y_continuous(
    expand = c(0.05, 0.05),
    breaks = seq(
      min(sel_range),
      max(sel_range),
      by = max(sel_range) / 4
    )
  ) +
  ggplot2::scale_fill_manual(
    values = palette_ecozones_labels
  ) +
  ggplot2::scale_color_manual(
    values = palette_ecozones_labels
  ) +
  ggplot2::labs(
    x = "",
    y = "Ratio of importance"
  )

plot_density <- function(sel_var = "human") {
  require(colorspace)
  p0 +
    ggplot2::facet_grid(
      region ~ predictor,
      switch = "x",
    ) +
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      strip.text.x = ggplot2::element_text(
        size = text_size, # [config criteria]
        color = common_gray # [config criteria]
      ),
      strip.text.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    ) +
    ggplot2::geom_hline(
      yintercept = seq(0, 1, 0.25),
      col = colorspace::lighten(common_gray, amount = 0.5), # [config criteria]
      linetype = 1,
      alpha = 0.5,
      size = line_size # [config criteria]
    ) +
    ggplot2::geom_density(
      data = data_spd_records %>%
        dplyr::filter(predictor == sel_var),
      mapping = ggplot2::aes(
        y = ratio_ind
      ),
      # width = .5,
      # .width = 0,
      trim = FALSE,
      fill = palette_predictors[sel_var], # [config criteria]
      col = NA
    ) +
    ggplot2::geom_segment(
      data = data_spd_region %>%
        dplyr::filter(
          predictor == sel_var
        ) %>%
        dplyr::filter(
          importance_type == "ratio_ind_wmean"
        ),
      mapping = ggplot2::aes(
        x = Inf,
        xend = -Inf,
        y = ratio,
        yend = ratio,
      ),
      lty = 1,
      linewidth = line_size * 10, # [config criteria]
      color = colorspace::darken(palette_predictors[sel_var], amount = 0.3)
    )
}

plot_summary <- function(sel_var = "human") {
  require(colorspace)
  p0 +
    ggplot2::facet_grid(
      region ~ climatezone_label,
      switch = "x",
      labeller = ggplot2::labeller(
        region = ggplot2::label_wrap_gen(7)
      )
    ) +
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(
        size = text_size, # [config criteria]
        color = common_gray # [config criteria]
      ),
      axis.text = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    ) +
    ggplot2::geom_hline(
      yintercept = seq(0, 1, 0.25),
      col = colorspace::lighten(common_gray, amount = 0.5), # [config criteria]
      linetype = 1,
      alpha = 0.5,
      size = line_size # [config criteria]
    ) +
    purrr::map(
      .x = c("95", "75", "50"),
      .f = ~ ggplot2::geom_segment(
        data = data_spd_records_quantiles %>%
          dplyr::filter(quantile_degree == .x) %>%
          dplyr::filter(
            predictor == sel_var
          ),
        mapping = ggplot2::aes(
          x = predictor,
          xend = predictor,
          y = upr,
          yend = lwr,
          col = climatezone_label
        ),
        alpha = 0.8,
        linewidth = (0.1 + (1 - (as.numeric(.x) / 100))) * 5
      )
    ) +
    ggplot2::geom_segment(
      data = data_spd_region %>%
        dplyr::filter(
          predictor == sel_var
        ) %>%
        dplyr::filter(
          importance_type == "ratio_ind_wmean"
        ),
      mapping = ggplot2::aes(
        x = Inf,
        xend = -Inf,
        y = ratio,
        yend = ratio,
      ),
      lty = 1,
      linewidth = line_size * 10, # [config criteria]
      color = colorspace::darken(palette_predictors[sel_var], amount = 0.3)
    ) +
    ggplot2::geom_point(
      data = data_spd_climatezone %>%
        dplyr::filter(
          predictor == sel_var
        ) %>%
        dplyr::filter(
          importance_type == "ratio_ind_wmean"
        ),
      mapping = ggplot2::aes(
        x = predictor,
        y = ratio,
        fill = climatezone_label
      ),
      shape = 21,
      col = common_gray, # [config criteria]
      size = point_size * 3, # [config criteria]
    )
}

fig_main <-
  cowplot::plot_grid(
    plot_density("human"),
    plot_summary("human") +
      ggplot2::theme(
        strip.text.y = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank()
      ),
    plot_density("climate") +
      ggplot2::theme(
        axis.title.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank()
      ),
    plot_summary("climate") +
      ggplot2::theme(
        axis.title.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank()
      ),
    ncol = 4,
    nrow = 1,
    align = "v",
    rel_widths = c(0.3, 0.95, 0.2, 1)
    # labels = c("Human", "", "Climate", "")
  )


#----------------------------------------------------------#
# 5. region maps -----
#----------------------------------------------------------#

datapoints <-
  data_meta %>%
  dplyr::select(dataset_id, long, lat, region, climatezone) %>%
  add_climatezone_as_factor() %>%
  dplyr::filter(dataset_id %in% output_spatial_spd$dataset_id)

get_points_to_map <- function(map, data_source) {
  map +
    ggplot2::geom_point(
      data = data_source,
      mapping = ggplot2::aes(
        x = long,
        y = lat,
        fill = climatezone
      ),
      size = point_size, # [config criteria]
      shape = 21,
      alpha = 0.5
    ) +
    ggplot2::theme(
      legend.position = "none"
    ) %>%
    return()
}

get_map_with_points <- function(sel_region, sel_alpha = 0.75) {
  get_map_region(
    rasterdata = data_geo_koppen,
    select_region = sel_region,
    sel_alpha = sel_alpha
  ) %>%
    get_points_to_map(
      data_source = datapoints %>%
        dplyr::filter(region == sel_region)
    ) %>%
    return()
}

fig_maps <-
  cowplot::plot_grid(
    get_map_with_points("North America"),
    get_map_with_points("Latin America"),
    get_map_with_points("Europe"),
    get_map_with_points("Asia"),
    get_map_with_points("Oceania"),
    ncol = 1
  )

legend_climatezones <-
  ggpubr::get_legend(
    get_map_with_points("Latin America") +
      ggplot2::theme(
        legend.position = "bottom",
        legend.title = ggplot2::element_blank()
      )
  )


#----------------------------------------------------------#
# 6. combine figures and save -----
#----------------------------------------------------------#
figure2 <-
  cowplot::plot_grid(
    cowplot::plot_grid(
      fig_main,
      fig_maps,
      ncol = 2,
      rel_widths = c(2, 0.5)
    ),
    ggpubr::as_ggplot(legend_climatezones),
    ncol = 1,
    rel_heights = c(1, 0.1)
  )

purrr::walk(
  .x = c("png", "pdf"),
  .f = ~ ggplot2::ggsave(
    paste(
      here::here("Outputs/Figures/Figure2_h1_spatial"),
      .x,
      sep = "."
    ),
    plot = figure2,
    width = image_width_vec["3col"], # [config criteria]
    height = 170,
    units = image_units, # [config criteria]
    bg = "white"
  )
)
