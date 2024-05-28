#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                      Hypothesis II
#
#
#                   O. Mottl, V.A. Felde
#                         2024
#
#----------------------------------------------------------#

#----------------------------------------------------------#
# 0. Load data -----
#----------------------------------------------------------#
# Load configuration
source(
  here::here(
    "R/00_Config_file.R"
  )
)

# - Load meta data
source(
  here::here(
    "R/main_analysis/02_meta_data.R"
  )
)


output_h2 <-
  targets::tar_read(
    name = "output_hvar_h2_spd",
    store = paste0(
      data_storage_path,
      "Targets_data/analyses_h2"
    )
  )

data_m2_filtered <-
  targets::tar_read(
    name = "data_m2_filtered",
    store = paste0(
      data_storage_path,
      "Targets_data/analyses_h2"
    )
  )
#----------------------------------------------------------#
# 1. Run and get scores dbRDA for visualisation -----
#----------------------------------------------------------#

data_to_plot_trajectory <-
  # add dbrda model to output_h2
  output_h2 %>%
  mutate(
    mod_dbrda = purrr::map2(
      .x = data_response_dist,
      .y = data_merge,
      .f = ~ run_dbrda(.x, .y)
    )
  ) %>%
  mutate(
    scores_dbrda = purrr::map(
      .x = mod_dbrda,
      .f = ~ get_scores_dbrda(.x)
    )
  ) %>%
  dplyr::select(
    region,
    climatezone,
    scores_dbrda
  ) %>%
  dplyr::left_join(
    data_meta %>%
      dplyr::select(
        region,
        climatezone,
        ecozone_koppen_5
      ) %>%
      dplyr::distinct(),
    dplyr::join_by(region, climatezone)
  ) %>%
  dplyr::mutate(
    region = factor(region,
      levels = vec_regions # [config criteria]
    ),
    ecozone_koppen_5 = factor(
      ecozone_koppen_5,
      levels = vec_climate_5 # [config criteria]
    )
  ) %>%
  tidyr::unnest(scores_dbrda) %>%
  add_climatezone_as_factor()


#----------------------------------------------------------#
# 2. Summary tables -----
#----------------------------------------------------------#
# all data
table_h2 <-
  # add dbrda model to output_h2
  output_h2 %>%
  mutate(
    mod_dbrda = purrr::map2(
      .x = data_response_dist,
      .y = data_merge,
      .f = ~ run_dbrda(.x, .y)
    )
  ) %>%
  mutate(
    scores_dbrda = purrr::map(
      .x = mod_dbrda,
      .f = ~ get_scores_dbrda(.x)
    )
  ) %>%
  dplyr::mutate(
    summary_table = purrr::map(
      .x = varhp,
      .f = ~ .x %>%
        purrr::pluck("summary_table")
    )
  ) %>%
  tidyr::unnest(summary_table) %>%
  dplyr::select(-c(data_merge, data_response_dist, varhp)) %>%
  dplyr::mutate(
    dplyr::across(
      .cols = Unique,
      .fns = ~ replace(., .x < 0, 0.0001)
    )
  ) %>% # negative variances can be ignored
  janitor::clean_names() %>%
  dplyr::group_by(
    region, climatezone
  ) %>%
  dplyr::mutate(
    sum_importance = sum(individual),
    ratio_unique = unique / sum_importance,
    ratio_ind = individual / sum_importance
  ) %>%
  dplyr::ungroup()

# reshape long formate
summary_h2_long <-
  table_h2 %>%
  dplyr::group_by(
    region,
    climatezone,
    predictor
  ) %>%
  # summarise by model weight
  dplyr::summarise(
    .groups = "drop",
    dplyr::across(
      dplyr::all_of(
        c(
          "ratio_unique",
          "ratio_ind"
        )
      ),
      list(
        wmean = ~ weighted.mean(
          x = .x,
          w = sum_importance,
          na.rm = TRUE
        )
      )
    )
  ) %>%
  tidyr::pivot_longer(
    dplyr::starts_with("ratio"),
    names_to = "importance_type",
    values_to = "ratio"
  ) %>%
  dplyr::mutate(
    region = factor(region,
      levels = vec_regions # [config criteria]
    )
  ) %>%
  add_climatezone_as_factor()


#----------------------------------------------------------#
# 3. Helper functions -----
#----------------------------------------------------------#

get_circe <- function(center = c(0, 0), radius = 1, npoints = 100) {
  tt <- seq(0, 2 * pi, length.out = npoints)
  xx <- center[1] + radius * cos(tt)
  yy <- center[2] + radius * sin(tt)

  data.frame(x = xx, y = yy) %>%
    return()
}

add_circe <- function(data_source,
                      line_col = "grey75",
                      line_type = 3,
                      ...) {
  data_source +
    ggplot2::geom_path(
      data = get_circe(...),
      mapping = ggplot2::aes(
        x = x,
        y = y,
      ),
      lty = line_type,
      col = line_col,
      linewidth = line_size # [config criteria]
    ) %>%
    return()
}

get_importance_fig <- function(
    data_source,
    sel_region, sel_climate, legend_position = "none") {
  data_source %>%
    dplyr::filter(
      region == sel_region &
        climatezone == sel_climate &
        importance_type == "ratio_ind_wmean"
    ) %>%
    dplyr::mutate(
      predictor = factor(
        predictor,
        levels = c("climate", "human")
      )
    ) %>%
    ggplot2::ggplot() +
    ggplot2::geom_bar(
      mapping = ggplot2::aes(
        y = ratio,
        x = climatezone,
        fill = predictor
      ),
      stat = "identity",
      width = 0.9,
      alpha = 1,
      position = "stack",
      show.legend = TRUE
    ) +
    ggplot2::scale_fill_manual(
      "Predictors",
      values = palette_predictors,
      drop = FALSE
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, 0.25),
      labels = seq(0, 1, 0.25)
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = legend_position,
      legend.title = ggplot2::element_text(
        size = text_size # [config criteria]
      ),
      legend.text = ggplot2::element_text(
        size = text_size # [config criteria]
      ),
      panel.background = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(
        fill = "transparent",
        color = NA
      ),
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      line = ggplot2::element_line(
        linewidth = line_size # [config criteria]
      ),
      axis.line = ggplot2::element_blank(),
      plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm")
    )
}

get_empty_fig <- function(line_col = "grey85", legend_position = "none", draw_circles = TRUE, axis_lim = 1.5) {
  fig <-
    ggplot2::ggplot() +
    ggplot2::geom_vline(
      xintercept = 0,
      linetype = 3,
      linewidth = line_size, # [config criteria]
      col = line_col
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = 3,
      linewidth = line_size, # [config criteria]
      col = line_col
    ) +
    ggplot2::theme(
      legend.position = legend_position,
      legend.title = ggplot2::element_text(
        size = text_size # [config criteria]
      ),
      legend.text = ggplot2::element_text(
        size = text_size # [config criteria]
      ),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      line = ggplot2::element_line(
        linewidth = line_size # [config criteria]
      ),
      text = ggplot2::element_text(size = text_size),
      panel.background = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(
        fill = "transparent",
        color = NA
      ),
      plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm"),
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank()
    ) +
    ggplot2::coord_fixed(
      xlim = c(-axis_lim, axis_lim),
      ylim = c(-axis_lim, axis_lim),
      expand = FALSE
    )

  if (
    isTRUE(draw_circles)
  ) {
    fig <-
      add_circe(fig, radius = 0.5, line_col = line_col) %>%
      add_circe(., radius = 1, line_col = line_col) %>%
      add_circe(., radius = 1.5, line_col = line_col)
  }

  return(fig)
}

get_trajectory_fig <- function(
    data_source,
    sel_region,
    sel_climate,
    ...) {
  require(ggnewscale)
  `%>%` <- magrittr::`%>%`

  data_biplot <-
    data_source %>%
    dplyr::filter(
      region == sel_region &
        climatezone == sel_climate &
        score == "biplot"
    ) %>%
    dplyr::mutate(
      pred_type = dplyr::case_when(
        .default = "climate",
        label == "spd" ~ "human"
      )
    )

  data_sites <-
    data_source %>%
    dplyr::filter(
      region == sel_region &
        climatezone == sel_climate &
        score == "sites"
    ) %>%
    dplyr::mutate(
      age = as.numeric(label) / 1000
    )

  fig <-
    get_empty_fig(
      line_col = "grey65",
      ...
    ) +
    ggplot2::geom_path(
      data = data_sites,
      mapping = ggplot2::aes(
        x = dbRDA1,
        y = dbRDA2,
        col = age
      ),
      lineend = "round",
      linejoin = "bevel",
      linewidth = 0.5,
      show.legend = TRUE
    ) +
    ggplot2::scale_color_gradient(
      "Age ka BP",
      low = paletete_age["young"], # [config criteria]
      high = paletete_age["old"] # [config criteria]
    ) +
    ggnewscale::new_scale_color() +
    ggplot2::geom_segment(
      data = data_biplot,
      mapping = ggplot2::aes(
        x = 0,
        y = 0,
        xend = dbRDA1,
        yend = dbRDA2,
        col = pred_type
      ),
      arrow = ggplot2::arrow(
        length = ggplot2::unit(0.03, "npc")
      ),
      linewidth = 0.75,
      show.legend = FALSE
    ) +
    ggplot2::scale_color_manual(
      "Predictors",
      values = palette_predictors, # [config criteria]
      drop = FALSE
    )

  return(fig)
}

get_traj_w_intset_fig <- function(sel_region, sel_climate) {
  `%>%` <- magrittr::`%>%`
  get_trajectory_fig(
    data_source = data_to_plot_trajectory,
    sel_region = sel_region,
    sel_climate = sel_climate,
    axis_lim = 1.5,
    legend_position = "none",
    draw_circles = TRUE
  ) +
    ggplot2::annotation_custom(
      grob = cowplot::as_grob(
        get_importance_fig(
          data_source = summary_h2_long,
          sel_region = sel_region,
          sel_climate = sel_climate
        )
      ),
      xmin = -1.45,
      xmax = -1,
      ymin = -1.6,
      ymax = 1.6
    ) %>%
    return()
}

#----------------------------------------------------------#
# 4. Trajectory plot -----
#----------------------------------------------------------#

data_plot_traj_intset <-
  tidyr::expand_grid(
    region = vec_regions, # [config criteria]
    climatezone = data_climate_zones$climatezone_label # [config criteria]
  ) %>%
  dplyr::mutate(
    list_fig_traj_intset = purrr::map2(
      .progress = TRUE,
      .x = region,
      .y = climatezone,
      .f = ~ get_traj_w_intset_fig(
        sel_region = .x,
        sel_climate = .y
      )
    )
  )
vec_emmpy_figs <-
  c(5, 8, 10, 13:17, 26:28, 30, 32, 37, 43, 45:50, 52:53, 55)

data_plot_traj_intset$list_fig_traj_intset[vec_emmpy_figs] <-
  purrr::map(
    .x = vec_emmpy_figs,
    .f = ~ get_empty_fig()
  )

figure_4_main <-
  cowplot::plot_grid(
    plotlist = data_plot_traj_intset$list_fig_traj_intset,
    nrow = length(vec_regions), # [config criteria]
    ncol = length(data_climate_zones$climatezone_label) # [config criteria]
  )

predictor_legend <-
  cowplot::get_legend(
    get_importance_fig(
      data_source = summary_h2_long,
      sel_region = "Europe",
      sel_climate = "Polar",
      legend_position = "bottom"
    )
  )

age_legend <-
  cowplot::get_legend(
    get_trajectory_fig(
      data_source = data_to_plot_trajectory,
      sel_region = "Europe",
      sel_climate = "Polar",
      draw_circles = FALSE,
      legend_position = "bottom"
    )
  )

guide_legend_predicor <-
  get_importance_fig(
    data_source = summary_h2_long,
    sel_region = "Europe",
    sel_climate = "Polar"
  ) +
  ggplot2::theme(
    axis.line.y = ggplot2::element_line(
      color = "#636363",
      linewidth = line_size
    ),
    axis.text.y = ggplot2::element_text(
      color = "#636363",
      size = text_size
    ),
    axis.title.y = ggplot2::element_text(
      color = "#636363",
      size = text_size,
      angle = 90
    ),
    axis.ticks.y = ggplot2::element_line(
      color = "#636363",
      linewidth = line_size,
      size = 0.5
    ),
    panel.grid.major.y = ggplot2::element_line(
      color = "#636363",
      linewidth = line_size # [config criteria]
    ),
    plot.margin = ggplot2::unit(c(0.1, 0.1, 0.1, 0.1), "cm")
  ) +
  ggplot2::labs(
    y = "Ration of importance"
  )

guide_legend_trajectory <-
  get_trajectory_fig(
    data_source = data_to_plot_trajectory,
    sel_region = "North America",
    sel_climate = "Cold - Cold Summer",
    draw_circles = TRUE
  )


#----------------------------------------------------------#
# 5. Save -----
#----------------------------------------------------------#

purrr::walk(
  .x = c("png", "pdf"),
  .f = ~ ggplot2::ggsave(
    paste(
      here::here("Outputs/Figures/Figure4_h2"),
      .x,
      sep = "."
    ),
    plot = figure_4_main,
    # make only 90% width of 3col so we have space for legend
    width = image_width_vec["3col"] * 0.9, # [config criteria]
    height = 140,
    units = image_units, # [config criteria]
    bg = "white"
  )
)

purrr::walk(
  .x = c("png", "pdf"),
  .f = ~ ggplot2::ggsave(
    paste(
      here::here("Outputs/Figures/Figure4_h2_legend_predictor"),
      .x,
      sep = "."
    ),
    plot = predictor_legend,
    width = image_width_vec["1col"], # [config criteria]
    height = 25,
    units = image_units, # [config criteria]
    bg = "white"
  )
)

purrr::walk(
  .x = c("png", "pdf"),
  .f = ~ ggplot2::ggsave(
    paste(
      here::here("Outputs/Figures/Figure4_h2_legend_age"),
      .x,
      sep = "."
    ),
    plot = age_legend,
    width = image_width_vec["2col"], # [config criteria]
    height = 25,
    units = image_units, # [config criteria]
    bg = "white"
  )
)

purrr::walk(
  .x = c("png", "pdf"),
  .f = ~ ggplot2::ggsave(
    paste(
      here::here("Outputs/Figures/Figure4_h2_guide_predictor"),
      .x,
      sep = "."
    ),
    plot = guide_legend_predicor,
    width = 25, # [config criteria]
    height = 50,
    units = image_units, # [config criteria]
    bg = "white"
  )
)

purrr::walk(
  .x = c("png", "pdf"),
  .f = ~ ggplot2::ggsave(
    paste(
      here::here("Outputs/Figures/Figure4_h2_guide_trajectory"),
      .x,
      sep = "."
    ),
    plot = guide_legend_trajectory,
    width = 50, # [config criteria]
    height = 50,
    units = image_units, # [config criteria]
    bg = "white"
  )
)
