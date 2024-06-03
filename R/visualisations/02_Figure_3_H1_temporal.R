#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                      Hypothesis I
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

# Load temporal spd
output_temporal_spd <-
  targets::tar_read(
    name = "output_temporal_spd",
    store = paste0(
      data_storage_path,
      "Targets_data/analyses_h1"
    )
  )

# Load temporal events
output_temporal_events <-
  targets::tar_read(
    name = "output_temporal_events",
    store = paste0(
      data_storage_path,
      "Targets_data/analyses_h1"
    )
  )

region_label_wrap <- 10


#----------------------------------------------------------#
# 1. Summary tables -----
#----------------------------------------------------------#
# spd
data_spd_temporal_summary_by_region <-
  output_temporal_spd %>%
  get_summary_tables(
    data_source = .,
    data_type = "temporal",
    group_var = c("region")
  )

# events
data_events_temporal_summary_by_region <-
  output_temporal_events %>%
  get_summary_tables(
    data_source = .,
    data_type = "temporal",
    group_var = c("region")
  )

# order factors by predictor and region
data_spd_region <-
  data_spd_temporal_summary_by_region %>%
  purrr::chuck("summary_table_weighted_mean") %>%
  filter(age > 1900) %>%
  add_predictor_as_factor() %>%
  add_region_as_factor() %>%
  dplyr::mutate(human_pred = "spd")

data_events_region <-
  data_events_temporal_summary_by_region %>%
  purrr::chuck("summary_table_weighted_mean") %>%
  add_predictor_as_factor() %>%
  add_region_as_factor() %>%
  dplyr::mutate(human_pred = "events")

data_combined_temporal <-
  dplyr::bind_rows(
    data_spd_region,
    data_events_region
  ) %>%
  dplyr::mutate(
    human_pred_width = dplyr::case_when(
      human_pred == "spd" ~ 1,
      human_pred == "events" ~ 0.5
    ),
    human_pred_col = dplyr::case_when(
      human_pred == "spd" ~ "grey80",
      human_pred == "events" ~ NA_character_
    )
  ) %>%
  dplyr::filter(
    importance_type == "ratio_ind_wmean"
  ) %>%
  add_age_as_factor()

#----------------------------------------------------------#
# 3. build figure -----
#----------------------------------------------------------#

palette_predictors_col <-
  c(
    palette_predictors,
    colorspace::lighten(palette_predictors, amount = 0.5)
  ) %>%
  rlang::set_names(
    nm = c(
      "Humans (SPD)",
      "Climate (SPD)",
      "Humans (Events)",
      "Climate (Events)"
    )
  )

main_temporal_fig2 <-
  data_combined_temporal %>%
  dplyr::mutate(
    pred_col = dplyr::case_when(
      predictor == "human" & human_pred == "spd" ~ "Humans (SPD)",
      predictor == "climate" & human_pred == "spd" ~ "Climate (SPD)",
      predictor == "human" & human_pred == "events" ~ "Humans (Events)",
      predictor == "climate" & human_pred == "events" ~ "Climate (Events)"
    )
  ) %>%
  ggplot2::ggplot() +
  ggplot2::facet_grid(
    region ~ age,
    switch = "both",
    labeller = ggplot2::labeller(
      region = ggplot2::label_wrap_gen(region_label_wrap)
    )
  ) +
  ggplot2::geom_bar(
    ggplot2::aes(
      x = human_pred,
      y = get("ratio"),
      fill = pred_col,
      color = human_pred,
    ),
    stat = "identity",
    col = NA,
    position = "stack",
    show.legend = TRUE
  ) +
  ggplot2::scale_y_continuous(
    position = "right",
    limits = c(0, 1),
    breaks = seq(0, 1, 0.5),
    oob = scales::squish
  ) +
  ggplot2::scale_x_discrete(
    limits = rev,
  ) +
  ggplot2::scale_fill_manual(
    values = palette_predictors_col,
    drop = FALSE
  ) +
  ggplot2::scale_colour_manual(
    values = c("events" = "grey80", "spd" = "grey30"),
    drop = FALSE
  ) +
  ggplot2::scale_alpha_manual(
    values = c("events" = 0.5, "spd" = 1),
    drop = FALSE
  ) +
  ggplot2::guides(
    colour = "none",
    alpha = "none",
    fill = ggplot2::guide_legend(
      title = "Predictors",
      title.position = "top",
      title.theme = ggplot2::element_text(
        size = text_size, # [config criteria]
        colour = common_gray
      ),
      label.theme = ggplot2::element_text(
        size = text_size, # [config criteria]
        colour = common_gray
      ),
      nrow = 2,
      ncol = 2,
      byrow = TRUE
    )
  ) +
  ggplot2::theme(
    legend.position = "bottom",
    legend.title = element_text(
      size = text_size, # [config criteria]
      colour = common_gray,
    ),
    legend.text = ggplot2::element_text(
      size = text_size, # [config criteria]
      colour = common_gray
    ),
    legend.margin = ggplot2::margin(
      t = 0,
      r = 0,
      b = 0,
      l = 0,
      unit = "pt"
    ),
    panel.background = ggplot2::element_blank(),
    panel.spacing.x = ggplot2::unit(0.5, "mm"),
    panel.spacing.y = grid::unit(3, "mm"),
    panel.grid.major = ggplot2::element_blank(),
    plot.background = ggplot2::element_rect(
      fill = "transparent",
      colour = NA
    ),
    strip.background = ggplot2::element_rect(
      fill = "transparent",
      colour = "transparent"
    ),
    strip.text = ggplot2::element_text(
      size = text_size, # [config criteria]
      colour = common_gray
    ),
    axis.title = ggplot2::element_text(
      size = text_size, # [config criteria]
      colour = common_gray
    ),
    axis.text = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    text = ggplot2::element_text(
      size = text_size, # [config criteria]
      colour = common_gray
    ),
    line = ggplot2::element_line(
      linewidth = line_size, # [config criteria]
      colour  = common_gray
    )
  ) +
  ggplot2::labs(
    x = "Age ka BP",
    y = "Ratio of importance"
  )

#----------------------------------------------------------#
# 4. save figure -----
#----------------------------------------------------------#
purrr::walk(
  .x = c("png", "pdf"),
  .f = ~ ggplot2::ggsave(
    paste(
      here::here("Outputs/Figures/Figure3_h1_temporal"),
      .x,
      sep = "."
    ),
    plot = main_temporal_fig2,
    width = image_width_vec["1col"], # [config criteria]
    height = 130,
    units = image_units, # [config criteria]
    bg = "white"
  )
)


# save table output
data_spd_temporal_summary_by_region %>%
  purrr::chuck("summary_table_weighted_mean") %>%
  dplyr::filter(importance_type == "ratio_ind_wmean") %>%
  tidyr::pivot_wider(
    names_from = region,
    values_from = ratio
  ) %>%
  dplyr::select(
    -importance_type
  ) %>%
  readr::write_csv(
    here::here("Outputs/Tables/summary_temporal_spd.csv")
  )

data_spd_temporal_summary_by_region %>%
  purrr::chuck("summary_table_weighted_mean") %>%
  dplyr::filter(importance_type == "ratio_ind_wmean") %>%
  dplyr::group_by(age, predictor) %>%
  dplyr::summarise(
    .groups = "drop",
    min = min(ratio),
    med = median(ratio),
    max = max(ratio)
  ) %>%
  readr::write_csv(
    here::here("Outputs/Tables/summary_temporal_spd_by_age.csv")
  )

# save table output
data_events_temporal_summary_by_region %>%
  purrr::chuck("summary_table_weighted_mean") %>%
  dplyr::filter(importance_type == "ratio_ind_wmean") %>%
  tidyr::pivot_wider(
    names_from = region,
    values_from = ratio
  ) %>%
  dplyr::select(
    -importance_type
  ) %>%
  readr::write_csv(
    here::here("Outputs/Tables/summary_temporal_events.csv")
  )

data_events_temporal_summary_by_region %>%
  purrr::chuck("summary_table_weighted_mean") %>%
  dplyr::filter(importance_type == "ratio_ind_wmean") %>%
  dplyr::group_by(age, predictor) %>%
  dplyr::summarise(
    min = min(ratio),
    med = median(ratio),
    max = max(ratio)
  ) %>%
  readr::write_csv(
    here::here("Outputs/Tables/summary_temporal_events_by_age.csv")
  )
