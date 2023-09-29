#----------------------------------------------------------#
#
#
#                   GlobalHumanImpact
#
#                    VISUALISATION
#           FIGURE 2: RESULTS H1 DETAILS ON HUMANS
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

# Import tables for plotting
source(
  here::here(
    "R/working_scripts/Results_script.R"
  )
)

# helper functions
get_human_unique_on_map <- function(
    data_source_varpar,
    data_source_geo,
    sel_region,
    sel_alpha = 0.3) {
  sel_data <-
    data_source_varpar %>%
    dplyr::filter(region == sel_region) %>%
    dplyr::filter(predictor == "human") %>%
    janitor::clean_names()

  sel_map <-
    get_map_region(
      rasterdata = data_source_geo,
      select_region = sel_region,
      sel_palette = palette_ecozones, # [config criteria]
      sel_alpha = sel_alpha
    )

  sel_map +
    ggplot2::geom_point(
      data = sel_data,
      mapping = ggplot2::aes(
        x = long,
        y = lat,
      ),
      size = 0.1,
      col = "gray30",
      shape = 20,
    ) +
    ggplot2::geom_point(
      data = sel_data,
      mapping = ggplot2::aes(
        x = long,
        y = lat,
        size = unique_percent,
        col = sel_classification
      ),
      shape = 21,
      fill = NA,
      show.legend = TRUE
    ) +
    ggplot2::scale_colour_manual(
      values = palette_ecozones # [config criteria]
    ) +
    ggplot2::scale_size_continuous(
      limits = c(0, 100)
    )
}

get_variability_prop_per_region <- function(
    data_source,
    sel_region,
    point_size = 2) {
  data_sel <-
    data_source %>%
    dplyr::filter(region == sel_region) %>%
    dplyr::mutate(sel_classification = as.factor(sel_classification)) %>%
    dplyr::full_join(
      data_climate_zones, # [config criteria]
      .,
      by = "sel_classification"
    )

  fig_basic <-
    data_sel %>%
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = 1,
        y = (constrained_eig / total_eig) * 100
      )
    ) +
    ggplot2::facet_wrap(~sel_classification, nrow = 1) +
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
      legend.position = "none",
      panel.spacing.x = grid::unit(0, "mm"),
      panel.border = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      plot.margin = grid::unit(c(0.1, 0.1, 0.1, 0.1), "mm")
    )

  fig_basic +
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
      data = data_sel %>%
        dplyr::group_by(sel_classification) %>%
        dplyr::summarise(
          median = median((constrained_eig / total_eig) * 100)
        ),
      mapping = ggplot2::aes(
        x = 1,
        y = median,
        fill = sel_classification
      ),
      shape = 22,
      col = "gray30",
      size = point_size
    )
}

get_plot_by_region <- function(data_source, sel_region) {
  data_source %>%
    dplyr::filter(region == sel_region) %>%
    purrr::chuck("plot", 1) %>%
    return()
}

#----------------------------------------------------------#
# 1. Load data -----
#----------------------------------------------------------#
data_geo_koppen <-
  readr::read_rds(
    paste0(
      data_storage_path,
      "Data/ecoregions2017/data_geo_koppen.rds"
    )
  ) %>%
  tibble::as_tibble() %>%
  dplyr::mutate(
    sel_classification = dplyr::case_when(
      ecozone_koppen_15 == "Cold_Without_dry_season" ~ ecozone_koppen_30,
      ecozone_koppen_5 == "Cold" ~ ecozone_koppen_15,
      ecozone_koppen_5 == "Temperate" ~ ecozone_koppen_15,
      .default = ecozone_koppen_5
    )
  )

#----------------------------------------------------------#
# 2. Data Wrangling -----
#----------------------------------------------------------#

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

data_proportion_variance <-
  output_h1_spatial %>%
  dplyr::mutate(
    summary_variation = purrr::map(
      .x = varhp,
      .f = ~ .x %>%
        purrr::pluck("summary_variation")
    )
  ) %>%
  dplyr::select(-c(data_merge, varhp)) %>%
  tidyr::unnest(summary_variation) %>%
  dplyr::left_join(
    data_meta %>%
      dplyr::mutate(
        sel_classification = dplyr::case_when(
          ecozone_koppen_15 == "Cold_Without_dry_season" ~ ecozone_koppen_30,
          ecozone_koppen_5 == "Cold" ~ ecozone_koppen_15,
          ecozone_koppen_5 == "Temperate" ~ ecozone_koppen_15,
          .default = ecozone_koppen_5
        )
      ) %>%
      dplyr::select(dataset_id, region, sel_classification),
    by = "dataset_id"
  ) %>%
  janitor::clean_names()



# get constrained spd scores
data_scores_nested <-
  output_h1_spatial %>%
  dplyr::mutate(
    constrained_scores = purrr::map(
      .x = data_merge,
      .f = purrr::possibly(
        get_scores_constrained_spd,
        otherwise = NA_real_
      )
    )
  ) %>%
  dplyr::inner_join(
    data_meta %>%
      dplyr::select(
        dataset_id,
        sel_classification,
        region
      ),
    by = "dataset_id"
  ) %>%
  dplyr::select(
    dataset_id,
    region,
    sel_classification,
    constrained_scores
  )

data_constrained_scores <-
  data_scores_nested %>%
  dplyr::mutate(
    scores = purrr::map(
      .x = constrained_scores,
      .f = ~ .x %>%
        purrr::pluck("scores")
    )
  ) %>%
  tidyr::unnest(scores) %>%
  dplyr::select(-constrained_scores)

data_adjr2 <-
  data_scores_nested %>%
  dplyr::mutate(
    adjr2 = purrr::map(
      .x = constrained_scores,
      .f = ~ .x %>%
        purrr::pluck("adjr2")
    )
  ) %>%
  tidyr::unnest(adjr2) %>%
  dplyr::select(-constrained_scores)

data_scores_merged <-
  dplyr::left_join(
    data_constrained_scores,
    data_adjr2,
    by = dplyr::join_by(
      dataset_id, region, sel_classification
    )
  )

#----------------------------------------------------------#
# 2. Figure maps -----
#----------------------------------------------------------#

data_fig_map <-
  tibble::tibble(
    region = c(
      "North America",
      "Latin America",
      "Europe",
      "Asia",
      "Oceania"
    )
  ) %>%
  dplyr::mutate(
    plot = purrr::map(
      .x = region,
      .f = ~ get_human_unique_on_map(
        data_source_varpar = data_spatial_vis,
        data_source_geo = data_geo_koppen,
        sel_region = .x
      )
    )
  )

#----------------------------------------------------------#
# 3. Figure total variability -----
#----------------------------------------------------------#

data_fig_variance <-
  tibble::tibble(
    region = c(
      "North America",
      "Latin America",
      "Europe",
      "Asia",
      "Oceania"
    )
  ) %>%
  dplyr::mutate(
    plot = purrr::map(
      .x = region,
      .f = ~ get_variability_prop_per_region(
        data_source = data_proportion_variance,
        sel_region = .x
      )
    )
  )

#----------------------------------------------------------#
# 2. Figure density -----
#----------------------------------------------------------#

# Density figures for full distribution of variance
data_fig_density <-
  dplyr::group_by(region) %>%
  tidyr::nest(data_dist = -c(region)) %>%
  dplyr::mutate(
    plot = purrr::map(
      .x = data_dist,
      .f = ~ ggplot2::ggplot(
        data = .x,
        mapping = ggplot2::aes(
          x = percentage
        )
      ) +
        ggplot2::facet_wrap(
          ~predictor,
          ncol = 3,
          scales = "free_y"
        ) +
        ggplot2::scale_colour_manual(
          values = palette_predictors_parts # [config criteria]
        ) +
        ggplot2::scale_fill_manual(
          values = palette_predictors_parts # [config criteria]
        ) +
        ggplot2::scale_x_continuous(
          limits = c(0, 100),
          breaks = c(seq(0, 100, by = 25))
        ) +
        ggplot2::scale_y_continuous(
          limits = c(0, NA)
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          legend.position = "none",
          plot.margin = grid::unit(c(0.1, 0.1, 0.1, 0), "mm"),
          strip.background = ggplot2::element_blank(),
          strip.text = ggplot2::element_blank(),
          axis.title = ggplot2::element_blank()
        ) +
        ggplot2::geom_density(
          mapping = ggplot2::aes(
            y = after_stat(count),
            col = var_part,
            fill = var_part
          ),
          alpha = 0.4
        )
    )
  )

#----------------------------------------------------------#
# 4. Figure ecosystem case scores -----
#----------------------------------------------------------#

data_fig_scores <-
  data_scores_merged %>%
  dplyr::filter(adjr2 > 0.1) %>%
  dplyr::group_by(region) %>%
  tidyr::nest(data_scores_merged = -c(region)) %>%
  dplyr::mutate(
    plot = purrr::map(
      .x = data_scores_merged,
      .f = ~ ggplot2::ggplot(data = .x, ggplot2::aes(
        x = age,
        y = CAP1,
        group = dataset_id
      )) +
        ggplot2::geom_line(
          ggplot2::aes(
            col = sel_classification
          ),
          alpha = 0.4,
          linewidth = 0.2
        ) +
        ggplot2::scale_colour_manual(
          values = palette_ecozones # [config criteria]
        ) +
        ggplot2::scale_y_continuous(limits = c(0, 5)) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          strip.text = ggplot2::element_text(size = 10),
          legend.position = "none",
          panel.grid.major = ggplot2::element_blank(),
          # panel.grid.minor = element_blank(),
          plot.background = ggplot2::element_blank(),
          plot.margin = grid::unit(c(0.2, 0.2, 0.2, 0), "mm"),
          axis.text.y = ggplot2::element_text(size = 6),
          axis.text.x = ggplot2::element_text(size = 6),
          axis.title = ggplot2::element_text(size = 8)
        ) +
        ggplot2::labs(
          y = "Scores",
          x = "Age BP"
        )
    )
  )

#----------------------------------------------------------#
# 5. Combine figures -----
#----------------------------------------------------------#

combined_detail_h1 <-
  cowplot::plot_grid(
    cowplot::plot_grid(
      get_plot_by_region(data_fig_map, "North America"),
      get_plot_by_region(data_fig_variance, "North America"),
      get_plot_by_region(data_fig_density, "North America"),
      get_plot_by_region(data_fig_scores, "North America"),
      nrow = 1
    ),
    cowplot::plot_grid(
      get_plot_by_region(data_fig_map, "Latin America"),
      get_plot_by_region(data_fig_variance, "Latin America"),
      get_plot_by_region(data_fig_density, "Latin America"),
      get_plot_by_region(data_fig_scores, "Latin America"),
      nrow = 1
    ),
    cowplot::plot_grid(
      get_plot_by_region(data_fig_map, "Europe"),
      get_plot_by_region(data_fig_variance, "Europe"),
      get_plot_by_region(data_fig_density, "Europe"),
      get_plot_by_region(data_fig_scores, "Europe"),
      nrow = 1
    ),
    cowplot::plot_grid(
      get_plot_by_region(data_fig_map, "Asia"),
      get_plot_by_region(data_fig_variance, "Asia"),
      get_plot_by_region(data_fig_density, "Asia"),
      get_plot_by_region(data_fig_scores, "Asia"),
      nrow = 1
    ),
    cowplot::plot_grid(
      get_plot_by_region(data_fig_map, "Oceania"),
      get_plot_by_region(data_fig_variance, "Oceania"),
      get_plot_by_region(data_fig_density, "Oceania"),
      get_plot_by_region(data_fig_scores, "Oceania"),
      nrow = 1
    ),
    ncol = 1
  )


ggsave(
  "combined_details_h1.pdf",
  plot = combined_detail_h1,
  width = 90,
  height = 90,
  units = "mm",
  bg = "white",
  scale = 2
)
