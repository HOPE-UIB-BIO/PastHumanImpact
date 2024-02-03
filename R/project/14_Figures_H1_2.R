#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                      Visualisation
#                      FIGURE 2 H1
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
      rasterdata = data_source_geo %>%
        dplyr::filter(region == sel_region),
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
        col = sel_classification
      ),
      size = 0.1,
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
      shape = 16,
      alpha = 0.3,
      show.legend = TRUE
    ) +
    ggplot2::scale_colour_manual(
      values = palette_ecozones # [config criteria]
    ) +
    ggplot2::scale_size_continuous(
      limits = c(0, 100),
      range = c(0.2, 5)
    )
}

get_unique_human_dist <- function(
    data_source,
    sel_region,
    point_size = 3) {
  data_work <-
    data_source %>%
    dplyr::mutate(sel_classification = as.factor(sel_classification)) %>%
    dplyr::full_join(
      data_climate_zones, # [config criteria]
      .,
      by = "sel_classification"
    )
  
  data_work %>%
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = 1,
        y = unique_percent
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
      alpha = 0.3,
      col = NA
    ) +
    ggplot2::geom_boxplot(
      fill = "white",
      col = "grey30",
      width = 0.1,
      outlier.shape = NA
    ) +
    ggplot2::geom_point(
      data = data_work %>%
        dplyr::group_by(sel_classification) %>%
        dplyr::summarise(
          unique_percent = median(unique_percent)
        ),
      mapping = ggplot2::aes(
        fill = sel_classification
      ),
      shape = 22,
      col = "gray30",
      size = point_size
    )
}

get_combined_row <- function(
    sel_region,
    sel_method = c("cowplot", "ggpubr"),
    sel_widths = c(1.2, 1, 1.5),
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
        get_plot_by_region(data_fig_unique_human_dist, sel_region),
        get_plot_by_region(data_fig_map, sel_region),
        fig_scores,
        nrow = 1,
        rel_widths = sel_widths
      )
  }
  
  if (
    sel_method == "ggpubr"
  ) {
    res <-
      ggpubr::ggarrange(
        get_plot_by_region(data_fig_unique_human_dist, sel_region),
        get_plot_by_region(data_fig_map, sel_region),
        fig_scores,
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
  ) %>%
  dplyr::distinct(x, y, .keep_all = TRUE) %>%
  dplyr::filter(
    x < 175.5 & x > -175 & y < 90 & y > -90
  ) %>%
  dplyr::mutate(
    long = x,
    lat = y
  ) %>%
  RUtilpol::geo_assign_value(
    data_source = .,
    dir = paste0(
      data_storage_path, "Data/Regions_shapefile/"
    ),
    sel_method = "shapefile",
    file_name = "Regions",
    var = "region",
    var_name = "region"
  )

#----------------------------------------------------------#
# 2. Data Wrangling -----
#----------------------------------------------------------#

# data unique human
data_unique_human <-
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
  dplyr::group_by(region) %>%
  tidyr::nest(
    data_to_plot = -c(region)
  ) %>%
  dplyr::ungroup()

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

data_score_mvpart <-
  data_scores_merged %>%
  dplyr::filter(adjr2 > 0.1) %>%
  dplyr::distinct(dataset_id, age, .keep_all = TRUE) %>%
  dplyr::select(region, dataset_id, CAP1, age) %>%
  tidyr::nest(data_nested = -c(region)) %>%
  dplyr::mutate(
    data_for_mvpart = purrr::map(
      .x = data_nested,
      .f = ~ .x %>%
        tidyr::pivot_wider(
          names_from = dataset_id,
          values_from = CAP1
        )
    )
  ) %>%
  dplyr::mutate(
    mvpart = purrr::map(
      .progress = TRUE,
      .x = data_for_mvpart,
      .f = ~ {
        data_mv <- .x %>%
          dplyr::select(-age)
        
        data_age <- .x %>%
          purrr::chuck("age")
        
        mvpart_result <-
          mvpart::mvpart(
            data.matrix(data_mv) ~ data_age,
            xv = "1se",
            xvmult = 1000,
            plot.add = FALSE,
            data = data_mv
          )
        
        utils::capture.output(
          change_points_age <-
            as.data.frame(
              summary(mvpart_result)$splits
            ) %>%
            purrr::pluck("index"),
          file = "NUL"
        )
        
        list(
          change_points_age = change_points_age,
          mvpart_result = mvpart_result
        ) %>%
          return()
      }
    )
  ) %>%
  dplyr::select(region, mvpart)

data_score_change_points <-
  data_score_mvpart %>%
  dplyr::mutate(
    change_points_age = purrr::map(
      .x = mvpart,
      .f = ~ .x %>%
        purrr::chuck("change_points_age")
    ),
    cv_error = purrr::map_dbl(
      .x = mvpart,
      .f = purrr::possibly(
        ~ .x %>%
          purrr::chuck("mvpart_result", "cptable") %>%
          as.data.frame() %>%
          dplyr::slice_tail(n = 1) %>%
          purrr::chuck("xerror"),
        otherwise = NA_real_
      )
    ),
    error = purrr::map_dbl(
      .x = mvpart,
      .f = purrr::possibly(
        ~ .x %>%
          purrr::chuck("mvpart_result", "cptable") %>%
          as.data.frame() %>%
          dplyr::slice_tail(n = 1) %>%
          purrr::chuck("rel error"),
        otherwise = NA_real_
      )
    ),
    r2 = 1 - error
  )

#----------------------------------------------------------#
# 3. Unique human -----
#----------------------------------------------------------#

data_fig_unique_human_dist <-
  data_unique_human %>%
  dplyr::mutate(
    plot = purrr::map(
      .x = data_to_plot,
      .f = ~ get_unique_human_dist(
        data_source = .x
      )
    )
  )

#----------------------------------------------------------#
# 4. Figure maps -----
#----------------------------------------------------------#

data_fig_map <-
  tibble::tibble(
    region = vec_regions # [config criteria]
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
# 5 Figure ecosystem case scores -----
#----------------------------------------------------------#

data_fig_scores <-
  data_scores_merged %>%
  dplyr::filter(adjr2 > 0.1) %>%
  dplyr::group_by(region) %>%
  tidyr::nest(data_scores_merged = -c(region)) %>%
  dplyr::left_join(
    data_score_change_points,
    by = dplyr::join_by(region)
  ) %>%
  dplyr::mutate(
    plot = purrr::pmap(
      .l = list(
        data_scores_merged,
        change_points_age,
        cv_error,
        r2
      ),
      .f = ~ ggplot2::ggplot(
        data = ..1,
        mapping = ggplot2::aes(
          x = age,
          y = CAP1,
          group = dataset_id
        )
      ) +
        ggplot2::geom_line(
          ggplot2::aes(
            col = sel_classification
          ),
          alpha = 0.7,
          linewidth = 0.2
        ) +
        ggplot2::geom_vline(
          xintercept = ..2,
          lty = 2,
          col = "grey30",
          linewidth = line_size * 2
        ) +
        ggplot2::geom_text(
          mapping = ggplot2::aes(
            y = 4.5,
            x = 8.5e3,
            label = paste("CV errror =", round(..3, 2))
          ),
          vjust = 0,
          hjust = 0,
          col = "grey30",
          size = text_size / 5 # [config criteria]
        ) +
        ggplot2::geom_text(
          mapping = ggplot2::aes(
            y = 4,
            x = 8.5e3,
            label = paste("R2 =", round(..4, 2))
          ),
          vjust = 0,
          hjust = 0,
          col = "grey30",
          size = text_size / 5 # [config criteria]
        ) +
        ggplot2::scale_colour_manual(
          values = palette_ecozones # [config criteria]
        ) +
        ggplot2::scale_y_continuous(limits = c(0, 5)) +
        ggplot2::scale_x_continuous(
          trans = "reverse",
          limits = c(8.5e3, 0),
          breaks = seq(8.5e3, 0, by = -2e3)
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
          axis.title = ggplot2::element_blank()
        ) +
        ggplot2::labs(
          y = "Scores",
          x = "Age BP"
        )
    )
  )

#----------------------------------------------------------#
# 6. Combine & save figures -----
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

combined_detail_h1_with_space_for_headings <-
  cowplot::plot_grid(
    patchwork::plot_spacer(),
    combined_detail_h1,
    ncol = 1,
    nrow = 2,
    rel_heights = c(1, 7)
  )

purrr::walk(
  .x = c("png", "pdf"),
  .f = ~ ggplot2::ggsave(
    paste(
      here::here("Outputs/combined_detail_h1"),
      .x,
      sep = "."
    ),
    plot = combined_detail_h1_with_space_for_headings,
    width = image_width_vec["2col"], # [config criteria]
    height = 130,
    units = image_units, # [config criteria]
    bg = "white"
  )
)

# end script
