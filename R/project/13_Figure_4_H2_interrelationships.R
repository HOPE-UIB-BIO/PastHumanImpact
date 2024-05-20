#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                      Hypothesis II
#
#
#                   O. Mottl, V.A. Felde
#                         2023
#
#----------------------------------------------------------#

#----------------------------------------------------------#
# 0. Load data -----
#----------------------------------------------------------#
# Load configuration
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


output_h2 <-
  targets::tar_read(
    name = "output_hvar_h2_spd",
    store = paste0(
      data_storage_path,
      "_targets_data/analyses_h2"
    )
  )

data_m2_filtered <-
  targets::tar_read(
    name = "data_m2_filtered",
    store = paste0(
      data_storage_path,
      "_targets_data/analyses_h2"
    )
  )
#----------------------------------------------------------#
# 1. Run and get scores dbRDA for visualisation -----
#----------------------------------------------------------#

add_climate_zone <- function(data_source) {
  data_source %>%
    dplyr::mutate(
      climatezone = dplyr::case_when(
        .default = climatezone,
        climatezone == "Cold_Without_dry_season_Very_Cold_Summer" ~ "Cold - Very Cold Summer",
        climatezone == "Cold_Without_dry_season_Cold_Summer" ~ "Cold - Cold Summer",
        climatezone == "Cold_Without_dry_season_Warm_Summer" ~ "Cold - Warm Summer",
        climatezone == "Cold_Without_dry_season_Hot_Summer" ~ "Cold - Hot Summer",
        climatezone == "Cold_Dry_Winter" ~ "Cold - Dry Winter",
        climatezone == "Cold_Dry_Summer" ~ "Cold - Dry Summer",
        climatezone == "Temperate_Without_dry_season" ~ "Temperate",
        climatezone == "Temperate_Dry_Winter" ~ "Temperate - Dry Winter",
        climatezone == "Temperate_Dry_Summer" ~ "Temperate - Dry Summer"
      ),
      climatezone = factor(
        climatezone,
        levels = data_climate_zones$climatezone_label # [config criteria]
      )
    )
}

# add dbrda model to output_h2
output_h2 <-
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
  )

data_to_plot_trajectory <-
  output_h2 %>%
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
  add_climate_zone()


#----------------------------------------------------------#
# 2. Summary tables -----
#----------------------------------------------------------#
# all data
table_h2 <-
  output_h2 %>%
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
  add_climate_zone()




#----------------------------------------------------------#
# 3. Ratio of predictor importance -----
#----------------------------------------------------------#

pred_importance_fig <-
  summary_h2_long %>%
  dplyr::mutate(
    predictor = factor(
      predictor,
      levels = c("human", "climate")
    )
  ) %>%
  dplyr::filter(
    importance_type == "ratio_ind_wmean"
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
    show.legend = FALSE
  ) +
  ggplot2::scale_fill_manual(
    values = palette_predictors,
    drop = FALSE
  ) +
  ggplot2::theme(
    aspect.ratio = 1 / 3,
    legend.position = "none",
    panel.background = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank(),
    strip.text.y = ggplot2::element_text(size = text_size), # [config criteria]
    strip.text.x = ggplot2::element_blank(),
    panel.spacing.x = ggplot2::unit(0.0, "lines"),
    panel.spacing.y = ggplot2::unit(0.1, "lines"),
    panel.grid.major = ggplot2::element_blank(),
    plot.background = ggplot2::element_rect(
      fill = "transparent",
      color = NA
    ),
    axis.title.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_text(size = text_size), # [config criteria]
    axis.text.x = ggplot2::element_text(
      size = text_size, # [config criteria]
      angle = 60, hjust = 1
    ),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    line = ggplot2::element_line(
      linewidth = line_size # [config criteria]
    ),
    plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm")
  ) +
  ggplot2::scale_y_continuous(
    limits = c(0, 1)
  ) +
  ggplot2::facet_grid(
    region ~ "1",
    labeller = ggplot2::labeller(
      region = ggplot2::label_wrap_gen(7)
    )
  ) +
  ggplot2::labs(
    x = "",
    y = "Ratio of importance"
  )


#----------------------------------------------------------#
# 4. Trajectory plot -----
#----------------------------------------------------------#

get_trajectory_figure <- function(data_source) {
  require(ggnewscale)

  data_biplot <-
    data_source %>%
    dplyr::filter(score %in% c("biplot")) %>%
    dplyr::mutate(
      pred_type = dplyr::case_when(
        .default = "climate",
        label == "spd" ~ "human"
      )
    )

  data_sites <-
    data_source %>%
    dplyr::filter(score %in% c("sites")) %>%
    dplyr::mutate(
      age = as.numeric(label) / 1000
    )

  fig <-
    ggplot2::ggplot() +
    ggplot2::coord_fixed(xlim = c(-2, 2)) +
    ggplot2::facet_grid(
      region ~ climatezone,
      labeller = ggplot2::labeller(
        region = ggplot2::label_wrap_gen(7),
        climatezone = ggplot2::label_wrap_gen(7)
      )
    ) +
    ggplot2::geom_vline(
      xintercept = 0,
      linetype = 3,
      linewidth = line_size, # [config criteria]
      col = "grey50"
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = 3,
      linewidth = line_size, # [config criteria]
      col = "grey50"
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
      linewidth = 0.5
    ) +
    ggplot2::scale_color_gradientn(
      "Age ka BP",
      colours = colorspace::sequential_hcl("BrwnYl", n = 12)
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
      linewidth = 0.75
    ) +
    ggplot2::scale_color_manual(
      "Predictors",
      values = palette_predictors, # [config criteria]
      drop = FALSE
    ) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = element_text(
        size = text_size # [config criteria]
      ),
      legend.text = ggplot2::element_text(
        size = text_size # [config criteria]
      ),
      line = ggplot2::element_line(
        linewidth = line_size # [config criteria]
      ),
      text = ggplot2::element_text(size = text_size),
      panel.background = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      strip.text.y = ggplot2::element_blank(),
      strip.text.x = ggplot2::element_text(
        size = text_size
      ),
      plot.background = ggplot2::element_rect(
        fill = "transparent",
        color = NA
      ),
      plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm"),
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank()
    )

  return(fig)
}


figure_trajectories <-
  get_trajectory_figure(data_to_plot_trajectory)

figure_4 <-
  cowplot::plot_grid(
    figure_trajectories,
    pred_importance_fig,
    nrow = 1,
    rel_heights = c(1, 2),
    rel_widths = c(1, 0.25)
  )



#----------------------------------------------------------#
# 7. Save -----
#----------------------------------------------------------#


# save predictor importance
purrr::walk(
  .x = c("png", "pdf"),
  .f = ~ ggplot2::ggsave(
    paste(
      here::here("Outputs/Figure4_predictor_importance"),
      .x,
      sep = "."
    ),
    plot = pred_importance_fig,
    width = image_width_vec["2col"], # [config criteria]
    height = 160,
    units = image_units, # [config criteria]
    bg = "white"
  )
)

# save trajectories
purrr::walk(
  .x = c("png", "pdf"),
  .f = ~ ggplot2::ggsave(
    paste(
      here::here("Outputs/Figure_trajectories"),
      .x,
      sep = "."
    ),
    plot = figure_trajectories,
    width = image_width_vec["3col"], # [config criteria]
    height = 220,
    units = image_units, # [config criteria]
    bg = "white"
  )
)

purrr::walk(
  .x = c("png", "pdf"),
  .f = ~ ggplot2::ggsave(
    paste(
      here::here("Outputs/Figure_4"),
      .x,
      sep = "."
    ),
    plot = figure_4,
    width = image_width_vec["3col"], # [config criteria]
    height = 220,
    units = image_units, # [config criteria]
    bg = "white"
  )
)
