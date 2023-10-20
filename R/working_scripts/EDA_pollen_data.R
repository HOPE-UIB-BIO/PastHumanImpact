#----------------------------------------------------------#
#
#
#                   GlobalHumanImpact
#
#                    Supplementary
#                 Pollen data overview
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

#----------------------------------------------------------#
# 1. Load data -----
#----------------------------------------------------------#

data_pollen <-
  targets::tar_read(
    name = data_assembly_filtered,
    store = external_storage_targets
  ) %>%
  dplyr::select(
    dataset_id,
    levels,
    counts_harmonised
  )

data_meta <-
  targets::tar_read(
    name = "data_meta",
    store = paste0(
      data_storage_path,
      "_targets_h1"
    )
  ) %>%
  dplyr::mutate(
    sel_classification = dplyr::case_when(
      ecozone_koppen_15 == "Cold_Without_dry_season" ~ ecozone_koppen_30,
      ecozone_koppen_5 == "Cold" ~ ecozone_koppen_15,
      ecozone_koppen_5 == "Temperate" ~ ecozone_koppen_15,
      .default = ecozone_koppen_5
    )
  ) %>%
  dplyr::filter(
    region != "Africa"
  ) %>%
  dplyr::mutate(sel_classification = as.factor(sel_classification)) %>%
  dplyr::inner_join(
    data_climate_zones, # [config criteria]
    .,
    by = "sel_classification"
  ) %>%
  dplyr::mutate(
    region = factor(region,
      levels = vec_regions # [config criteria]
    )
  )

#----------------------------------------------------------#
# 2. Spatial distribution -----
#----------------------------------------------------------#


fig_map <-
  data_pollen %>%
  dplyr::inner_join(
    data_meta,
    by = "dataset_id"
  ) %>%
  ggplot2::ggplot(
    mapping = ggplot2::aes(
      x = long,
      y = lat
    )
  ) +
  ggplot2::scale_fill_manual(
    values = palette_ecozones # [config criteria]
  ) +
  ggplot2::scale_color_manual(
    values = palette_ecozones # [config criteria]
  ) +
  ggplot2::theme_classic() +
  ggplot2::theme(
    text = ggplot2::element_text(
      size = text_size # [config criteria]
    ),
    line = ggplot2::element_line(
      linewidth = line_size # [config criteria]
    ),
    legend.position = "none",
    plot.margin = grid::unit(c(0.1, 0.1, 0.1, 0.1), "mm")
  ) +
  ggplot2::coord_equal(
    ratio = 1.3,
    ylim = range(data_meta$lat),
    xlim = range(data_meta$long)
  ) +
  ggplot2::labs(
    title = "A) Spatial coverage and distribution of records",
    x = expression(
      paste(
        "Longitude ", (degree ~ E)
      )
    ),
    y = expression(
      paste(
        "Latitude ", (degree ~ N)
      )
    )
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq(-180, 180, by = 50)
  ) +
  ggplot2::scale_y_continuous(
    breaks = seq(-90, 90, by = 15)
  ) +
  ggplot2::geom_polygon(
    data = ggplot2::map_data("world") %>%
      dplyr::filter(lat > -60 & lat < 85),
    ggplot2::aes(
      group = group
    ),
    fill = "grey80",
    alpha = 0.4
  ) +
  ggplot2::geom_point(
    mapping = ggplot2::aes(
      col = sel_classification
    ),
    size = 3,
    shape = 19,
    alpha = 0.1
  ) +
  ggplot2::geom_point(
    mapping = ggplot2::aes(
      col = sel_classification
    ),
    size = 0.5,
    shape = 20,
    alpha = 1
  ) +
  ggplot2::geom_point(
    col = "grey30",
    size = 0.1,
    shape = 20,
    alpha = 1
  )

#----------------------------------------------------------#
# 3. Record count -----
#----------------------------------------------------------#

fig_recod_count <-
  data_pollen %>%
  dplyr::inner_join(
    data_meta,
    by = "dataset_id"
  ) %>%
  dplyr::group_by(region, sel_classification) %>%
  dplyr::count(
    name = "n_records"
  ) %>%
  dplyr::mutate(
    region = factor(
      region,
      levels = c(
        "North America",
        "Europe",
        "Asia",
        "Latin America",
        "Oceania"
      )
    )
  ) %>%
  ggplot2::ggplot(
    mapping = ggplot2::aes(
      y = n_records,
      x = sel_classification,
      col = sel_classification,
      fill = sel_classification
    )
  ) +
  ggplot2::facet_wrap(
    ~region,
    nrow = 2,
    dir = "h"
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
    plot.caption.position = "panel",
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(
      size = text_size,
      hjust = 0.01
    ),
    axis.text.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    legend.position = "none",
    plot.margin = grid::unit(c(0.1, 0.1, 0.1, 0.1), "mm")
  ) +
  ggplot2::labs(
    y = "Number of records",
    title = "B) Number of records in each climate zone"
  ) +
  ggplot2::geom_segment(
    mapping = ggplot2::aes(
      xend = sel_classification,
      yend = 0
    ),
    col = "grey30"
  ) +
  ggplot2::geom_point(
    size = 3,
    shape = 21,
    col = "grey30"
  ) +
  ggplot2::geom_text(
    mapping = ggplot2::aes(
      label = n_records
    ),
    nudge_y = 20,
    col = "grey30",
    size = text_size / 3
  )

#----------------------------------------------------------#
# 4. Climate tone legend -----
#----------------------------------------------------------#

fig_color_legend <-
  data_meta %>%
  dplyr::distinct(sel_classification) %>%
  dplyr::mutate(
    climate_zone_name = dplyr::case_when(
      .default = sel_classification,
      sel_classification == "Cold_Without_dry_season_Very_Cold_Summer" ~ "Cold - without dry season - very cold summer",
      sel_classification == "Cold_Without_dry_season_Cold_Summer" ~ "Cold - without dry season - cold summer",
      sel_classification == "Cold_Without_dry_season_Warm_Summer" ~ "Cold - without dry season - warm summer",
      sel_classification == "Cold_Without_dry_season_Hot_Summer" ~ "Cold - without dry season - hot summer",
      sel_classification == "Cold_Dry_Winter" ~ "Cold - dry winter",
      sel_classification == "Cold_Dry_Summer" ~ "Cold - dry summer",
      sel_classification == "Temperate_Without_dry_season" ~ "Temperate - without dry season",
      sel_classification == "Temperate_Dry_Winter" ~ "Temperate - dry winter",
      sel_classification == "Temperate_Dry_Summer" ~ "Temperate - dry summer"
    ),
    climate_zone_name = factor(
      climate_zone_name,
      levels = c(
        "Arid",
        "Tropical",
        "Temperate - dry summer",
        "Temperate - dry winter",
        "Temperate - without dry season",
        "Cold - dry summer",
        "Cold - dry winter",
        "Cold - without dry season - hot summer",
        "Cold - without dry season - warm summer",
        "Cold - without dry season - cold summer",
        "Cold - without dry season - very cold summer",
        "Polar"
      )
    )
  ) %>%
  ggplot2::ggplot(
    mapping = ggplot2::aes(
      x = 1,
      y = climate_zone_name,
      label = climate_zone_name,
      col = sel_classification,
      fill = sel_classification
    )
  ) +
  ggplot2::coord_cartesian(
    xlim = c(0, 20)
  ) +
  ggplot2::scale_fill_manual(
    values = palette_ecozones # [config criteria]
  ) +
  ggplot2::scale_color_manual(
    values = palette_ecozones # [config criteria]
  ) +
  ggplot2::theme_void() +
  ggplot2::theme(
    text = ggplot2::element_text(
      size = text_size # [config criteria]
    ),
    line = ggplot2::element_line(
      linewidth = line_size # [config criteria]
    ),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    plot.caption.position = "panel",
    legend.position = "none",
    plot.margin = grid::unit(c(0.1, 0.1, 0.1, 0.1), "mm")
  ) +
  ggplot2::labs(
    title = "C) Climate zone color legend"
  ) +
  ggplot2::geom_point(
    shape = 22,
    col = "gray30",
    size = 10
  ) +
  ggplot2::geom_text(
    mapping = ggplot2::aes(
      x = 2
    ),
    col = "grey30",
    size = text_size / 3,
    hjust = 0
  )

#----------------------------------------------------------#
# 3. Temporal distribution -----
#----------------------------------------------------------#

fig_temporal <-
  data_pollen %>%
  tidyr::unnest(levels) %>%
  dplyr::inner_join(
    data_meta,
    by = "dataset_id"
  ) %>%
  dplyr::group_by(
    region, sel_classification, dataset_id
  ) %>%
  dplyr::summarise(
    .groups = "drop",
    age_min = min(age),
    age_max = max(age),
    age_mean = mean(age)
  ) %>%
  ggplot2::ggplot(
    mapping = ggplot2::aes(
      x = age_min,
      xend = age_max,
      y = reorder(dataset_id, age_mean),
      yend = reorder(dataset_id, age_mean),
      col = sel_classification
    )
  ) +
  ggplot2::facet_wrap(
    scales = "free_y",
    ~region,
    ncol = 1
  ) +
  ggplot2::scale_x_continuous(
    trans = "reverse"
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
    plot.caption.position = "panel",
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(
      size = text_size,
      hjust = 0.01
    ),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_blank(),
    legend.position = "none",
    plot.margin = grid::unit(c(0.1, 0.1, 0.1, 0.1), "mm")
  ) +
  ggplot2::labs(
    title = "D) Temporal coverage of records",
    x = "Age (cal yr BP)"
  ) +
  ggplot2::geom_segment(
    alpha = 0.5,
    linewidth = 0.5
  )

#----------------------------------------------------------#
# 4. taxonomic richness -----
#----------------------------------------------------------#

data_pollen_taxa <-
  data_pollen %>%
  dplyr::mutate(
    taxa = purrr::map(
      .progress = TRUE,
      .x = counts_harmonised,
      .f = ~ .x %>%
        dplyr::select(-sample_id) %>%
        names()
    )
  ) %>%
  dplyr::select(
    dataset_id, taxa
  )


data_pollen_taxa_n_per_dataset <-
  data_pollen_taxa %>%
  dplyr::mutate(
    n_taxa = purrr::map_dbl(
      .progress = TRUE,
      .x = taxa,
      .f = ~ length(.x)
    )
  ) %>%
  dplyr::inner_join(
    data_meta,
    by = "dataset_id"
  ) %>%
  dplyr::mutate(
    region = factor(
      region,
      levels = c(
        "North America",
        "Europe",
        "Asia",
        "Latin America",
        "Oceania"
      )
    )
  )

data_pollen_taxa_per_continnt <-
  data_pollen_taxa %>%
  tidyr::unnest(taxa) %>%
  dplyr::inner_join(
    data_meta,
    by = "dataset_id"
  ) %>%
  dplyr::distinct(region, sel_classification, taxa) %>%
  dplyr::group_by(region, sel_classification) %>%
  dplyr::count(
    name = "n_taxa"
  ) %>%
  dplyr::mutate(
    region = factor(
      region,
      levels = c(
        "North America",
        "Europe",
        "Asia",
        "Latin America",
        "Oceania"
      )
    )
  )


fig_taxa_basic <-
  tibble::tibble() %>%
  ggplot2::ggplot(
    mapping = ggplot2::aes(
      y = n_taxa,
      x = sel_classification,
      col = sel_classification,
      fill = sel_classification
    )
  ) +
  ggplot2::facet_wrap(
    ~region,
    nrow = 2,
    dir = "h"
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
    plot.caption.position = "panel",
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(
      size = text_size,
      hjust = 0.01
    ),
    axis.text.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    legend.position = "none",
    plot.margin = grid::unit(c(0.1, 0.1, 0.1, 0.1), "mm")
  )

fig_taxa_continent <-
  fig_taxa_basic +
  ggplot2::labs(
    y = "Number of taxa",
    title = "E) Total number of taxa in each climate zone"
  ) +
  ggplot2::geom_segment(
    data = data_pollen_taxa_per_continnt,
    mapping = ggplot2::aes(
      xend = sel_classification,
      yend = 0
    ),
    col = "grey30"
  ) +
  ggplot2::geom_point(
    data = data_pollen_taxa_per_continnt,
    size = 3,
    shape = 21,
    col = "grey30"
  ) +
  ggplot2::geom_text(
    data = data_pollen_taxa_per_continnt,
    mapping = ggplot2::aes(
      label = n_taxa
    ),
    nudge_y = 70,
    col = "grey30",
    size = text_size / 3
  )


fig_taxa_per_record <-
  fig_taxa_basic +
  ggplot2::labs(
    y = "Number of taxa",
    title = "F) The number of taxa per record in each climate zone"
  ) +
  ggplot2::geom_jitter(
    data = data_pollen_taxa_n_per_dataset,
    alpha = 0.3
  ) +
  ggplot2::geom_violin(
    data = data_pollen_taxa_n_per_dataset,
    alpha = 0.3,
    col = NA
  ) +
  ggplot2::geom_boxplot(
    data = data_pollen_taxa_n_per_dataset,
    fill = "white",
    col = "grey30",
    width = 0.1,
    outlier.shape = NA
  ) +
  ggplot2::geom_point(
    data = data_pollen_taxa_n_per_dataset %>%
      dplyr::group_by(region, sel_classification) %>%
      dplyr::summarise(
        median = median(n_taxa)
      ),
    mapping = ggplot2::aes(
      y = median
    ),
    shape = 22,
    col = "gray30",
    size = 3
  )



#----------------------------------------------------------#
# 5. Merge -----
#----------------------------------------------------------#

fig_merge <-
  cowplot::plot_grid(
    cowplot::plot_grid(
      fig_map,
      fig_recod_count,
      fig_color_legend,
      rel_widths = c(1, 0.7, 0.7),
      nrow = 1
    ),
    fig_temporal,
    cowplot::plot_grid(
      fig_taxa_continent,
      fig_taxa_per_record,
      nrow = 1
    ),
    nrow = 3,
    rel_heights = c(0.2, 0.5, 0.3)
  )

purrr::walk(
  .x = c("png", "pdf"),
  .f = ~ ggplot2::ggsave(
    paste(
      here::here("Outputs/Supp/Pollen_data"),
      .x,
      sep = "."
    ),
    plot = fig_merge,
    width = image_width_vec["3col"], # [config criteria]
    height = 300,
    units = image_units, # [config criteria]
    bg = "white",
    scale = 1.2
  )
)
