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
      text = ggplot2::element_text(
        size = text_size # [config criteria]
      ),
      line = ggplot2::element_line(
        linewidth = line_size # [config criteria]
      ),
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

get_combined_row <- function(
    sel_region,
    sel_method = c("cowplot", "ggpubr"),
    sel_widths = c(0.2, 0.2, 0.2, 0.1),
    remove = "x") {
  sel_method <- match.arg(sel_method)

  if (
    "x" %in% remove
  ) {
    fig_scores <-
      get_plot_by_region(data_fig_scores, sel_region) +
      ggpubr::rremove("x.text") +
      ggpubr::rremove("x.ticks") +
      ggpubr::rremove("x.title")
  } else {
    fig_scores <- get_plot_by_region(data_fig_scores, sel_region)
  }

  if (
    sel_method == "cowplot"
  ) {
    res <-
      cowplot::plot_grid(
        get_plot_by_region(data_fig_map, sel_region),
        fig_scores,
        get_plot_by_region(data_fig_density, sel_region) +
          ggpubr::rremove("xy.title"),
        get_plot_by_region(data_fig_variance, sel_region) +
          ggpubr::rremove("y.text") +
          ggpubr::rremove("y.ticks"),
        nrow = 1,
        rel_widths = sel_widths
      )
  }

  if (
    sel_method == "ggpubr"
  ) {
    res <-
      ggpubr::ggarrange(
        get_plot_by_region(data_fig_map, sel_region),
        fig_scores,
        get_plot_by_region(data_fig_density, sel_region) +
          ggpubr::rremove("xy.title"),
        get_plot_by_region(data_fig_variance, sel_region) +
          ggpubr::rremove("y.text") +
          ggpubr::rremove("y.ticks"),
        nrow = 1,
        widths = sel_widths
      )
  }
  return(res)
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

data_fig_variance$plot[[1]]

#----------------------------------------------------------#
# 2. Figure density -----
#----------------------------------------------------------#

# Density figures for full distribution of variance
data_fig_density <-
  data_dist %>%
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
          scales = "free_x"
        ) +
        ggplot2::coord_flip() +
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
          text = ggplot2::element_text(
            size = text_size # [config criteria]
          ),
          line = ggplot2::element_line(
            linewidth = line_size # [config criteria]
          ),
          legend.position = "none",
          plot.margin = grid::unit(c(0.1, 0.1, 0.1, 0), "mm"),
          panel.grid.minor = ggplot2::element_blank(),
          panel.grid.major.x = ggplot2::element_blank(),
          strip.background = ggplot2::element_blank(),
          strip.text = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank()
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
          alpha = 0.4
        )
    )
  )

data_fig_density$plot[[1]]

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
        ggplot2::scale_x_continuous(
          limits = c(0, 8.5e3),
          breaks = seq(0, 8.5e3, by = 2e3)
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
          panel.grid.minor = ggplot2::element_blank(),
          plot.background = ggplot2::element_blank(),
          plot.margin = grid::unit(c(0.2, 0.2, 0.2, 0), "mm"),
          axis.title.y = ggplot2::element_blank()
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

sel_teselation_method <- "cowplot"

combined_detail_h1 <-
  cowplot::plot_grid(
    get_combined_row("North America", sel_teselation_method),
    get_combined_row("Latin America", sel_teselation_method),
    get_combined_row("Europe", sel_teselation_method),
    get_combined_row("Asia", sel_teselation_method),
    get_combined_row("Oceania", sel_teselation_method, remove = NULL),
    ncol = 1
  )

purrr::walk(
  .x = c("png", "pdf"),
  .f = ~ ggplot2::ggsave(
    paste(
      here::here("Outputs/combined_detail_h1"),
      .x,
      sep = "."
    ),
    plot = combined_detail_h1,
    width = image_width_vec["2col"], # [config criteria]
    height = 120,
    units = image_units, # [config criteria]
    bg = "white"
  )
)
