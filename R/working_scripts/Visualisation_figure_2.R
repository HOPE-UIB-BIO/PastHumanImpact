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

#----------------------------------------------------------#
# 2. Estimate averages -----
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

add_climatezone_as_factor <- function(data_source) {
  data_source %>%
    dplyr::mutate(
      climatezone = as.factor(climatezone),
      region = factor(
        region,
        levels = vec_regions # [config criteria]
      )
    ) %>%
    dplyr::full_join(
      data_climate_zones, # [config criteria]
      .,
      by = "climatezone"
    ) %>%
    return()
}

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
  )

summary(data_spd_records_quantiles)

#----------------------------------------------------------#
# 4. Build figure -----
#----------------------------------------------------------#

sel_range <- c(0, 2)

p0 <-
  tibble::tibble() %>%
  ggplot2::ggplot() +
  ggplot2::coord_cartesian(
    ylim = sel_range
  ) +
  ggplot2::theme(
    legend.position = "none"
  ) +
  ggplot2::scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(
      min(sel_range),
      max(sel_range),
      by = max(sel_range) / 4
    )
  ) +
  ggplot2::scale_fill_manual(
    values = palette_ecozones
  ) +
  ggplot2::scale_color_manual(
    values = palette_ecozones
  ) +
  ggplot2::labs(
    x = "",
    y = ""
  )

plot_density <- function(sel_var = "human") {
  p0 +
    ggplot2::facet_wrap(~region, ncol = 1) +
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
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
      fill = "grey",
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
      lty = 3
    )
}

plot_summary <- function(sel_var = "human") {
  p0 +
    ggplot2::facet_grid(region ~ climatezone) +
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
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
          col = climatezone
        ),
        alpha = 0.3,
        linewidth = (0.5 + (1 - (as.numeric(.x) / 100))) * 2
      )
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
        col = climatezone
      ),
      size = 3,
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
      lty = 3
    )
}

cowplot::plot_grid(
  plot_density("human"),
  plot_summary("human"),
  plot_density("climate"),
  plot_summary("climate"),
  ncol = 4,
  nrow = 1,
  align = "v",
  rel_widths = c(0.3, 1, 0.3, 1),
  labels = c("Human", "", "Climate", "")
)
