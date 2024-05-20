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
        climatezone == "Cold_Without_dry_season_Very_Cold_Summer" ~ "Cold - very cold summer",
        climatezone == "Cold_Without_dry_season_Cold_Summer" ~ "Cold - cold summer",
        climatezone == "Cold_Without_dry_season_Warm_Summer" ~ "Cold - warm summer",
        climatezone == "Cold_Without_dry_season_Hot_Summer" ~ "Cold - hot - summer",
        climatezone == "Cold_Dry_Winter" ~ "Cold - dry winter",
        climatezone == "Cold_Dry_Summer" ~ "Cold - dry summer",
        climatezone == "Temperate_Without_dry_season" ~ "Temperate",
        climatezone == "Temperate_Dry_Winter" ~ "Temperate - dry winter",
        climatezone == "Temperate_Dry_Summer" ~ "Temperate - dry summer"
      ),
      climatezone = factor(
        climatezone,
        levels = levels(
          data_climate_zones
        ) # [config criteria]
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

data_to_plot_trajectory$climatezone %>%
  unique()

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
    # strip.text.y = ggplot2::element_blank(),
    strip.text.x = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank(),
    plot.background = ggplot2::element_rect(
      fill = "transparent",
      color = NA
    ),
    axis.title.x = ggplot2::element_text(size = 8),
    axis.title.y = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(size = 8, angle = 60, hjust = 1),
    axis.text.y = ggplot2::element_text(size = 6),
    plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm")
  ) +
  ggplot2::scale_y_continuous(limits = c(0, 1)) +
  ggplot2::facet_wrap(
    ~region,
    ncol = 1
  ) +
  ggplot2::labs(x = "")

pred_importance_fig



#----------------------------------------------------------#
# 4. Trajectory plot -----
#----------------------------------------------------------#


get_trajectory_figure <- function(data_source) {
  data_biplot <-
    data_source %>%
    dplyr::filter(score %in% c("biplot"))

  data_sites <-
    data_source %>%
    dplyr::filter(score %in% c("sites"))

  fig <-
    ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = data_biplot,
      mapping = ggplot2::aes(
        x = 0,
        y = 0,
        xend = dbRDA1,
        yend = dbRDA2
      ),
      arrow = ggplot2::arrow(length = ggplot2::unit(0.03, "npc")),
      col = "black"
    ) +
    ggplot2::geom_text(
      data = data_biplot,
      mapping = ggplot2::aes(
        x = dbRDA1 * 1.2,
        y = dbRDA2 * 1.2,
        label = label
      ),
      size = 3,
      col = "black"
    ) +
    ggplot2::geom_path(
      data = data_sites,
      mapping = ggplot2::aes(
        x = dbRDA1,
        y = dbRDA2,
        col =  as.numeric(label)
      ),
      lineend = "round",
      linejoin = "bevel",
      linewidth = 0.5
      # arrow = arrow(
      #   length = unit(0.1, "inches"),
      #   ends = "first",
      #   type = "open")
    ) +
    ggplot2::geom_point(
      data = data_sites,
      mapping = ggplot2::aes(
        x = dbRDA1,
        y = dbRDA2,
        col = as.numeric(label)
      ),
      size = 1.5
    ) +
    ggplot2::geom_text(
      data = data_sites %>%
        dplyr::mutate(label = as.character(as.numeric(label) / 1000)),
      mapping = ggplot2::aes(
        x = dbRDA1,
        y = dbRDA2,
        label = label,
        col = as.numeric(label) * 1000
      ),
      size = 3,
      vjust = 1.5,
      col = "black",
      check_overlap = TRUE
    ) +
    ggplot2::geom_vline(
      xintercept = 0,
      linetype = 2,
      linewidth = 0.1,
      col = "grey50"
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = 2,
      linewidth = 0.1,
      col = "grey50"
    ) +
    ggplot2::coord_fixed(xlim = c(-2, 2)) +
    ggplot2::scale_color_gradientn(
      colours = colorspace::sequential_hcl("BrwnYl", n = 12)
    ) +
    ggplot2::theme(
      legend.position = "bottom",
      panel.background = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      # strip.text.y = ggplot2::element_blank(),
      # strip.text.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(
        fill = "transparent",
        color = NA
      ),
      axis.title.x = ggplot2::element_text(size = 8),
      axis.title.y = ggplot2::element_text(size = 8),
      axis.text.x = ggplot2::element_text(size = 8, angle = 60),
      axis.text.y = ggplot2::element_text(size = 8),
      plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm")
    ) +
    ggplot2::facet_grid(
      region ~ climatezone,
    ) +
    ggplot2::labs(x = "dbRDA 1", y = "dbRDA 2", color = "Age BP")

  return(fig)
}

data_to_plot_trajectory %>%
  filter(region == "North America") %>%
  get_trajectory_figure()

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



##########################################################################
