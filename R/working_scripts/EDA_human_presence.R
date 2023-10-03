#----------------------------------------------------------#
#
#
#                   GlobalHumanImpact
#
#                    Supplementary
#                 EDA of human presence
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

# remotes::install_github("hrbrmstr/waffle") # nolint
library(waffle)

#----------------------------------------------------------#
# 1. Load data -----
#----------------------------------------------------------#

data_c14 <-
  targets::tar_read(
    name = "data_c14",
    store = paste0(
      data_storage_path,
      "_targets_h1"
    )
  )

data_c14_subset <-
  targets::tar_read(
    name = "data_c14_subset",
    store = paste0(
      data_storage_path,
      "_targets_h1"
    )
  )

data_events <-
  targets::tar_read(
    name = "data_events_to_fit",
    store = paste0(
      data_storage_path,
      "_targets_h1"
    )
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
  dplyr::mutate(
    sel_classification = as.factor(sel_classification)
  ) %>%
  dplyr::full_join(
    data_climate_zones, # [config criteria]
    .,
    by = "sel_classification"
  ) %>%
  dplyr::mutate(
    region = factor(region,
      levels = vec_regions # [config criteria]
    )
  ) %>%
  dplyr::filter(
    region != "Africa"
  ) %>%
  dplyr::select(region, sel_classification, dataset_id, long, lat)

#----------------------------------------------------------#
# 2. Number of RC dates -----
#----------------------------------------------------------#

# assign the region
data_c14_continents <-
  data_c14 %>%
  janitor::clean_names() %>%
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

# load KG climate translation table
koppen_tranlation_table <-
  readr::read_csv(
    paste0(
      data_storage_path, "Data/ecoregions2017/koppen_link.csv"
    )
  )

# assign the KG climate zones
data_c14_climate_zones <-
  data_c14_continents %>%
  janitor::clean_names() %>%
  RUtilpol::geo_assign_value(
    data_source = .,
    dir = paste0(
      data_storage_path, "Data/ecoregions2017/"
    ),
    sel_method = "tif",
    file_name = "Beck_KG_V1_present_0p083",
    var = "raster_values",
    var_name = "koppen_raste_value"
  ) %>%
  dplyr::left_join(koppen_tranlation_table,
    by = c("koppen_raste_value" = "raster_values")
  ) %>%
  dplyr::select(-koppen_raste_value) %>%
  dplyr::rename(
    ecozone_koppen_30 = genzone,
    ecozone_koppen_15 = genzone_cluster,
    ecozone_koppen_5 = broadbiome
  ) %>%
  dplyr::mutate(
    sel_classification = dplyr::case_when(
      ecozone_koppen_15 == "Cold_Without_dry_season" ~ ecozone_koppen_30,
      ecozone_koppen_5 == "Cold" ~ ecozone_koppen_15,
      ecozone_koppen_5 == "Temperate" ~ ecozone_koppen_15,
      .default = ecozone_koppen_5
    )
  )

fig_n_c14 <-
  data_c14_climate_zones %>%
  dplyr::filter(age < 12.5e3) %>%
  tidyr::drop_na(region, sel_classification) %>%
  dplyr::mutate(sel_classification = as.factor(sel_classification)) %>%
  dplyr::full_join(
    data_climate_zones, # [config criteria]
    .,
    by = "sel_classification"
  ) %>%
  dplyr::mutate(
    region = factor(region,
      levels = vec_regions # [config criteria]
    )
  ) %>%
  dplyr::filter(
    region != "Africa"
  ) %>%
  REcopol:::add_age_bin(
    bin_size = 500
  ) %>%
  dplyr::group_by(
    region, sel_classification, BIN
  ) %>%
  dplyr::count() %>%
  ggplot2::ggplot() +
  ggplot2::facet_wrap(
    ~region,
    ncol = 1,
    scales = "free_y"
  ) +
  ggplot2::scale_color_manual(
    values = palette_ecozones
  ) +
  ggplot2::scale_fill_manual(
    values = palette_ecozones
  ) +
  ggplot2::scale_x_continuous(
    limits = c(0, 12.5e3),
    breaks = seq(0, 12.5e3, by = 1e3),
    labels = seq(0, 12.5, by = 1)
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "bottom",
    plot.caption.position = "panel",
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(
      size = text_size,
      hjust = 0.01
    ),
    panel.grid.minor = ggplot2::element_blank()
  ) +
  ggplot2::labs(
    x = "Age (ka uncal yr)",
    y = "Number of RC dates",
    col = "Climate zone",
    fill = "Climate zone",
    caption = "The Y-axis is scaled for each region"
  ) +
  ggplot2::geom_area(
    mapping = ggplot2::aes(
      x = BIN,
      y = n,
      fill = sel_classification
    ),
    alpha = 1
  )

# alternative figure
if (
  FALSE
) {
  fig_n_c14 <-
    data_c14_climate_zones %>%
    dplyr::filter(age < 12.5e3) %>%
    tidyr::drop_na(region, sel_classification) %>%
    dplyr::mutate(
      region = factor(region,
        levels = vec_regions # [config criteria]
      )
    ) %>%
    dplyr::filter(
      region != "Africa"
    ) %>%
    ggplot2::ggplot() +
    ggplot2::facet_wrap(
      ~region,
      ncol = 1
    ) +
    ggplot2::scale_color_manual(
      values = palette_ecozones
    ) +
    ggplot2::scale_fill_manual(
      values = palette_ecozones
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, 12.5e3),
      breaks = seq(0, 12.5e3, by = 1e3),
      labels = seq(0, 12.5, by = 1)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 10),
      breaks = seq(0, 10, by = 2.5),
      labels = seq(0, 4000, by = 1000)
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(
        size = text_size,
        hjust = 0.01
      ),
      panel.grid.minor = ggplot2::element_blank()
    ) +
    ggplot2::geom_density(
      mapping = ggplot2::aes(
        x = age,
        y = ggplot2::after_stat(count),
        fill = sel_classification,
        col = sel_classification
      ),
      alpha = 0.2
    )
}

purrr::walk(
  .x = c("png", "pdf"),
  .f = ~ ggplot2::ggsave(
    paste(
      here::here("Outputs/Supp/C14_N"),
      .x,
      sep = "."
    ),
    plot = fig_n_c14,
    width = image_width_vec["3col"], # [config criteria]
    height = 180,
    units = image_units, # [config criteria]
    bg = "white"
  )
)

#----------------------------------------------------------#
# 3. N of valid RC -----
#----------------------------------------------------------#
data_valid_n_rc_raw <-
  dplyr::left_join(
    data_meta,
    data_c14_subset,
    by = "dataset_id"
  ) %>%
  dplyr::mutate(
    has_rc = purrr::map_lgl(
      .x = rc,
      .f = ~ is.data.frame(.x)
    )
  ) %>%
  dplyr::mutate(
    n_rc = purrr::map2_dbl(
      .x = has_rc,
      .y = rc,
      .f = ~ ifelse(.x, nrow(.y), 0)
    )
  ) %>%
  dplyr::mutate(
    has_valid_n_rc = purrr::map_lgl(
      .x = n_rc,
      .f = ~ .x >= 50
    )
  ) %>%
  dplyr::distinct(
    region, sel_classification, dataset_id, has_valid_n_rc
  ) %>%
  tidyr::drop_na(region, sel_classification)


data_valid_n_rc <-
  data_valid_n_rc_raw %>%
  tidyr::drop_na(region, sel_classification) %>%
  dplyr::group_by(region, sel_classification, has_valid_n_rc) %>%
  dplyr::count(
    name = "N"
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(N = as.double(N)) %>%
  dplyr::arrange(
    region, sel_classification, has_valid_n_rc
  ) %>%
  tidyr::complete(
    region,
    sel_classification,
    has_valid_n_rc,
    fill = list(N = 0.00001)
  )


fig_valid_n_rc <-
  data_valid_n_rc %>%
  ggplot2::ggplot() +
  ggplot2::facet_grid(
    region ~ sel_classification
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.title = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.caption.position = "panel",
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(
      size = text_size,
      hjust = 0.01
    ),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank()
  ) +
  ggplot2::labs(
    fill = "Has enough RC dates to construct SPD?"
  ) +
  ggplot2::coord_equal() +
  waffle::geom_waffle(
    mapping = ggplot2::aes(
      fill = has_valid_n_rc,
      values = N
    ),
    size = 1,
    col = NA,
    n_rows = 15,
    make_proportional = FALSE
  )


purrr::walk(
  .x = c("png", "pdf"),
  .f = ~ ggplot2::ggsave(
    paste(
      here::here("Outputs/Supp/C14_valid_datasets"),
      .x,
      sep = "."
    ),
    plot = fig_valid_n_rc,
    width = image_width_vec["3col"], # [config criteria]
    height = 180,
    units = image_units, # [config criteria]
    bg = "white"
  )
)


#----------------------------------------------------------#
# 4. Human presence detected -----
#----------------------------------------------------------#

data_id_has_human_impact <-
  data_events %>%
  dplyr::filter(
    !var_name %in% c("bi", "no_impact")
  ) %>%
  tidyr::unnest(data_to_fit) %>%
  dplyr::filter(value == 1) %>%
  dplyr::distinct(dataset_id) %>%
  dplyr::mutate(
    have_events = TRUE
  )

data_valid_events_raw <-
  data_meta %>%
  dplyr::left_join(
    data_id_has_human_impact,
    by = "dataset_id"
  ) %>%
  dplyr::mutate(
    have_events = ifelse(is.na(have_events), FALSE, have_events)
  )

data_valid_events <-
  data_valid_events_raw %>%
  dplyr::group_by(region, sel_classification, have_events) %>%
  dplyr::count(
    name = "N"
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(N = as.double(N)) %>%
  dplyr::arrange(
    region, sel_classification, have_events
  ) %>%
  tidyr::complete(
    region,
    sel_classification,
    have_events,
    fill = list(N = 0.00001)
  )

fig_human_presence_detected <-
  data_valid_events %>%
  ggplot2::ggplot() +
  ggplot2::facet_grid(
    region ~ sel_classification
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.title = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.caption.position = "panel",
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(
      size = text_size,
      hjust = 0.01
    ),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank()
  ) +
  ggplot2::labs(
    fill = "Human presence detected in pollen data?"
  ) +
  ggplot2::coord_equal() +
  waffle::geom_waffle(
    mapping = ggplot2::aes(
      fill = have_events,
      values = N
    ),
    size = 1,
    col = NA,
    n_rows = 15,
    make_proportional = FALSE
  )

purrr::walk(
  .x = c("png", "pdf"),
  .f = ~ ggplot2::ggsave(
    paste(
      here::here("Outputs/Supp/Events_valid_datasets"),
      .x,
      sep = "."
    ),
    plot = fig_human_presence_detected,
    width = image_width_vec["3col"], # [config criteria]
    height = 180,
    units = image_units, # [config criteria]
    bg = "white"
  )
)

#----------------------------------------------------------#
# 5. Status of data based on human presence and RC -----
#----------------------------------------------------------#
fig_human_presence_status <-
  dplyr::full_join(
    data_valid_n_rc_raw,
    data_valid_events_raw,
    by = dplyr::join_by(region, sel_classification, dataset_id)
  ) %>%
  dplyr::mutate(
    status = dplyr::case_when(
      have_events == TRUE & has_valid_n_rc == TRUE ~
        "human presence & enough RC",
      have_events == TRUE & has_valid_n_rc == FALSE ~
        "human presence but not enough RC",
      have_events == FALSE & has_valid_n_rc == TRUE ~
        "no human presence but enough RC",
      .default = "no human presence & not enough RC"
    )
  ) %>%
  dplyr::mutate(
    status = factor(
      status,
      levels = c(
        "human presence & enough RC",
        "human presence but not enough RC",
        "no human presence but enough RC",
        "no human presence & not enough RC"
      )
    )
  ) %>%
  dplyr::group_by(region, sel_classification, status) %>%
  dplyr::count(
    name = "N"
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(N = as.double(N)) %>%
  dplyr::arrange(
    region, sel_classification, status
  ) %>%
  tidyr::complete(
    region,
    sel_classification,
    status,
    fill = list(N = 0.00001)
  ) %>%
  ggplot2::ggplot() +
  ggplot2::facet_grid(
    region ~ sel_classification
  ) +
  ggplot2::theme_bw() +
  ggplot2::guides(
    fill = ggplot2::guide_legend(nrow = 2)
  ) +
  ggplot2::theme(
    axis.title = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.caption.position = "panel",
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(
      size = text_size,
      hjust = 0.01
    ),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank()
  ) +
  ggplot2::labs(
    caption = "Human presence is detected from pollen data.",
    fill = ""
  ) +
  ggplot2::coord_equal() +
  waffle::geom_waffle(
    mapping = ggplot2::aes(
      fill = status,
      values = N
    ),
    size = 1,
    col = NA,
    n_rows = 15,
    make_proportional = FALSE
  )

purrr::walk(
  .x = c("png", "pdf"),
  .f = ~ ggplot2::ggsave(
    paste(
      here::here("Outputs/Supp/Events_and_RC_status"),
      .x,
      sep = "."
    ),
    plot = fig_human_presence_status,
    width = image_width_vec["3col"], # [config criteria]
    height = 180,
    units = image_units, # [config criteria]
    bg = "white"
  )
)


#----------------------------------------------------------#
# 6. Temporal trends of human impact  -----
#----------------------------------------------------------#

list_fig_event_temporal_trends <-
  vec_regions %>%
  purrr::map(
    .f = ~ plot_data_events(
      data_source_events = data_events,
      select_region = .x
    )
  )

purrr::iwalk(
  .progress = TRUE,
  .x = list_fig_event_temporal_trends,
  .f = ~ ggplot2::ggsave(
    paste0(
      here::here("Outputs/Supp/Events_temporal_"),
      .y,".png"
    ),
    plot = .x,
    width = image_width_vec["2col"], # [config criteria]
    height = 120,
    units = image_units, # [config criteria]
    bg = "white"
  )
)
