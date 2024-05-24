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

# - Load meta data
source(
  here::here(
    "R/project/02_meta_data.R"
  )
)

#----------------------------------------------------------#
# 1. Load data -----
#----------------------------------------------------------#

data_pollen <-
  targets::tar_read(
    name = data_assembly_filtered,
    store = paste0(
      data_storage_path,
      "_targets_data/pipeline_pollen_data"
    )
  ) %>%
  dplyr::select(
    dataset_id,
    levels,
    counts_harmonised
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
  dplyr::filter(
    region != "Africa"
  ) %>%
  add_climatezone_as_factor() %>%
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
      size = text_size, # [config criteria]
      color = common_gray # [config criteria]
    ),
    line = ggplot2::element_line(
      linewidth = line_size, # [config criteria]
      color = common_gray # [config criteria]
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
      col = climatezone
    ),
    size = 3,
    shape = 19,
    alpha = 0.2
  ) +
  ggplot2::geom_point(
    mapping = ggplot2::aes(
      col = climatezone
    ),
    size = 0.5,
    shape = 20,
    alpha = 1
  ) +
  ggplot2::geom_point(
    col = common_gray,
    size = 0.1,
    shape = 20,
    alpha = 1
  )

legend_climatezones <-
  ggpubr::get_legend(
    fig_map +
      ggplot2::geom_point(
        mapping = ggplot2::aes(
          col = climatezone
        ),
        size = 3,
        shape = 19,
        alpha = 1
      ) +
      ggplot2::theme(
        legend.position = "right",
        legend.title = ggplot2::element_blank()
      )
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
  dplyr::filter(
    region != "Africa"
  ) %>%
  dplyr::group_by(region, climatezone) %>%
  dplyr::count(
    name = "n_records"
  ) %>%
  add_climatezone_as_factor() %>% # [config criteria]
  add_region_as_factor() %>% # [config criteria]
  ggplot2::ggplot(
    mapping = ggplot2::aes(
      y = n_records,
      x = climatezone,
      col = climatezone,
      fill = climatezone
    )
  ) +
  ggplot2::facet_wrap(
    ~region,
    nrow = 1,
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
      size = text_size, # [config criteria]
      color = common_gray # [config criteria]
    ),
    line = ggplot2::element_line(
      linewidth = line_size, # [config criteria]
      color = common_gray # [config criteria]
    ),
    plot.caption.position = "panel",
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = common_gray, # [config criteria]
      hjust = 0.01
    ),
    axis.text.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    legend.position = "none",
    plot.margin = grid::unit(c(0.1, 0.1, 0.1, 0.1), "mm")
  ) +
  ggplot2::labs(
    y = "N records",
    title = "B) Number of records and pollen taxa"
  ) +
  ggplot2::geom_segment(
    mapping = ggplot2::aes(
      xend = climatezone,
      yend = 0
    ),
    col = common_gray # [config criteria]
  ) +
  ggplot2::geom_point(
    size = 3,
    shape = 21,
    col = common_gray # [config criteria]
  ) +
  ggplot2::geom_text(
    mapping = ggplot2::aes(
      label = n_records
    ),
    nudge_y = 20,
    col = common_gray, # [config criteria]
    size = text_size / 3
  )

#----------------------------------------------------------#
# 4. Climate tone legend -----
#----------------------------------------------------------#

fig_color_legend <-
  data_meta %>%
  add_climatezone_as_factor() %>% # [config criteria]
  tidyr::drop_na(climatezone) %>%
  dplyr::distinct(climatezone) %>%
  ggplot2::ggplot(
    mapping = ggplot2::aes(
      x = 1,
      y = climatezone,
      label = climatezone,
      col = climatezone,
      fill = climatezone
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
      size = text_size, # [config criteria]
      color = common_gray # [config criteria]
    ),
    line = ggplot2::element_line(
      linewidth = line_size, # [config criteria]
      color = common_gray # [config criteria]
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
    col = common_gray, # [config criteria]
    size = 10
  ) +
  ggplot2::geom_text(
    mapping = ggplot2::aes(
      x = 2
    ),
    col = common_gray, # [config criteria]
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
  dplyr::filter(
    region != "Africa"
  ) %>%
  dplyr::group_by(
    region, climatezone, dataset_id
  ) %>%
  add_climatezone_as_factor() %>% # [config criteria]
  add_region_as_factor() %>% # [config criteria]
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
      col = climatezone,
    )
  ) +
  ggplot2::facet_wrap(
    scales = "free_y",
    ~region,
    nrow = 1
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
      size = text_size, # [config criteria]
      color = common_gray # [config criteria]
    ),
    line = ggplot2::element_line(
      linewidth = line_size, # [config criteria]
      color = common_gray # [config criteria]
    ),
    plot.caption.position = "panel",
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = common_gray, # [config criteria]
      hjust = 0.01
    ),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_blank(),
    legend.position = "none",
    plot.margin = grid::unit(c(0.1, 0.1, 0.1, 5), "mm")
  ) +
  ggplot2::labs(
    title = "C) Temporal coverage of records",
    x = "Age (cal ka BP)"
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
  dplyr::filter(
    region != "Africa"
  )  %>%
  add_climatezone_as_factor() %>% # [config criteria]
  add_region_as_factor() # [config criteria]


data_pollen_taxa_per_continnt <-
  data_pollen_taxa %>%
  tidyr::unnest(taxa) %>%
  dplyr::inner_join(
    data_meta,
    by = "dataset_id"
  ) %>%
  dplyr::filter(
    region != "Africa"
  )  %>%
  dplyr::distinct(region, climatezone, taxa) %>%
  dplyr::group_by(region, climatezone) %>%
  dplyr::count(
    name = "n_taxa"
  ) %>%
  add_climatezone_as_factor() %>% # [config criteria]
  add_region_as_factor() %>% # [config criteria]
 tidyr::drop_na(
   climatezone, region
 )


fig_taxa_basic <-
  tibble::tibble() %>%
  ggplot2::ggplot(
    mapping = ggplot2::aes(
      y = n_taxa,
      x = climatezone,
      col = climatezone,
      fill = climatezone
    )
  ) +
  ggplot2::facet_wrap(
    ~region,
    nrow = 1,
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
      size = text_size, # [config criteria]
      color = common_gray # [config criteria]
    ),
    line = ggplot2::element_line(
      linewidth = line_size, # [config criteria]
      color = common_gray # [config criteria]
    ),
    plot.caption.position = "panel",
    strip.background = ggplot2::element_blank(),
    # strip.text = ggplot2::element_text(
    #   size = text_size, # [config criteria]
    #   color = common_gray, # [config criteria]
    #   hjust = 0.01
    # ),
    strip.text = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    legend.position = "none",
    plot.margin = grid::unit(c(0.1, 0.1, 0.1, 0.1), "mm")
  )

fig_taxa_continent <-
  fig_taxa_basic +
  ggplot2::labs(
    y = "Total N taxa"
   # title = "D) Total number of taxa in each climate zone"
  ) +
  ggplot2::geom_segment(
    data = data_pollen_taxa_per_continnt,
    mapping = ggplot2::aes(
      xend = climatezone,
      yend = 0
    ),
    col = common_gray # [config criteria]
  ) +
  ggplot2::geom_point(
    data = data_pollen_taxa_per_continnt,
    size = 3,
    shape = 21,
    col = common_gray # [config criteria]
  ) +
  ggplot2::geom_text(
    data = data_pollen_taxa_per_continnt,
    mapping = ggplot2::aes(
      label = n_taxa
    ),
    nudge_y = 70,
    col = common_gray, # [config criteria],
    size = text_size / 3
  )


fig_taxa_per_record <-
  fig_taxa_basic +
  ggplot2::labs(
    y = "N taxa per record",
    #title = "C) The number of taxa per record in each climate zone"
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
    col = common_gray, # [config criteria]
    width = 0.1,
    outlier.shape = NA
  ) +
  ggplot2::geom_point(
    data = data_pollen_taxa_n_per_dataset %>%
      dplyr::group_by(region, climatezone) %>%
      dplyr::summarise(
        median = median(n_taxa)
      ),
    mapping = ggplot2::aes(
      y = median
    ),
    shape = 22,
    col = common_gray, # [config criteria]
    size = 3
  )



#----------------------------------------------------------#
# 5. Merge -----
#----------------------------------------------------------#

fig_merge <-
  cowplot::plot_grid(
    cowplot::plot_grid(
    fig_map,
    ggpubr::as_ggplot(legend_climatezones),
    rel_widths = c(1, 0.3),
    nrow = 1
    ),
    cowplot::plot_grid(
      fig_recod_count,
      fig_taxa_per_record,
      fig_taxa_continent,
      rel_heights = c(1, 0.8, 0.8),
      nrow = 3
    ),
    fig_temporal,
    nrow = 3,
    rel_heights = c(1, 1.5, 0.6)
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
    bg = "white"
  )
)
