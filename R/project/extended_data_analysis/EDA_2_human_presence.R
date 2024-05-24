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
    "R/project/00_Config_file.R"
  )
)

# remotes::install_github("hrbrmstr/waffle") # nolint
library(waffle)


#----------------------------------------------------------#
# 1. Load data -----
#----------------------------------------------------------#

# - Load meta data
source(
  here::here(
    "R/project/02_meta_data.R"
  )
)

# - load c14 data
data_c14 <-
  paste0(
    data_storage_path,
    "Data/c14/data_rc_2022-11-29.rds"
  ) %>%
  get_file_from_path()

# - get polygons for each dataset_id
data_polygons <-
  get_polygons(
    data_source = data_meta,
    distance_buffer = 10 # 10° away from site
  )

# - subset C14 data for each dataset_id and calculate distance to it
data_c14_subset <-
  subset_c14_data(
    data_source_c14 = data_c14,
    data_source_polygons = data_polygons,
    data_source_meta = data_meta
  )

data_events <-
  targets::tar_read(
    name = "data_events_to_fit",
    store = paste0(
      data_storage_path,
      "_targets_data/pipeline_events"
    )
  )


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
    var = "region"
  )

# load KG climate translation table
koppen_tranlation_table <-
  readr::read_csv(
    paste0(
      data_storage_path, "Data/ecoregions2017/koppen_link.csv"
    ),
    show_col_types = FALSE
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
    var = "raster_values"
  ) %>%
  dplyr::left_join(koppen_tranlation_table,
    by = c("raster_values")
  ) %>%
  dplyr::select(-raster_values) %>%
  dplyr::rename(
    ecozone_koppen_30 = genzone,
    ecozone_koppen_15 = genzone_cluster,
    ecozone_koppen_5 = broadbiome
  ) %>%
  dplyr::mutate(
    climatezone = dplyr::case_when(
      ecozone_koppen_15 == "Cold_Without_dry_season" ~ ecozone_koppen_30,
      ecozone_koppen_5 == "Cold" ~ ecozone_koppen_15,
      ecozone_koppen_5 == "Temperate" ~ ecozone_koppen_15,
      .default = ecozone_koppen_5
    )
  ) %>%
  add_climatezone_as_factor() %>%
  dplyr::filter(
    region != "Africa"
  ) 



fig_n_c14 <-
  data_c14_climate_zones %>%
  dplyr::filter(age < 12.5e3) %>%
  tidyr::drop_na(region, climatezone) %>%
  add_region_as_factor() %>%
  add_climatezone_as_factor() %>%
  REcopol:::add_age_bin(
    bin_size = 500
  ) %>%
  dplyr::group_by(
    region, climatezone, BIN
  ) %>%
  dplyr::count() %>%
  ggplot2::ggplot() +
  ggplot2::facet_wrap(
    ~region,
    ncol = 1,
    scales = "free_y",
    labeller = ggplot2::labeller( 
      region = ggplot2::label_wrap_gen(7)
    ) 
  ) +
  ggplot2::scale_color_manual(
    values = palette_ecozones
  ) +
  ggplot2::scale_fill_manual(
    values = palette_ecozones
  ) +
  ggplot2::scale_x_continuous(
    trans = "reverse",
    limits = c(12.5e3, 0),
    breaks = seq(12.5e3, 0, by = -1e3),
    labels = seq(12.5, 0, by = -1)
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "right",
    plot.caption.position = "panel",
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = common_gray, # [config criteria]
      hjust = 0.01
    ),
    panel.grid.minor = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = common_gray # [config criteria]
    ),
    legend.title = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = common_gray # [config criteria]
    ),
    text = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = common_gray # [config criteria]
    ),
    axis.text.y = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = common_gray # [config criteria]
    ),
    axis.title.y = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = common_gray # [config criteria]
    ),
    line = ggplot2::element_line(
      linewidth = line_size, # [config criteria]
      color = common_gray # [config criteria]
    )
  )+
  ggplot2::labs(
    x = "Age (uncal ka BP)",
    y = "Number of RC dates",
    col = "Climate zone",
    fill = "Climate zone",
    caption = "The Y-axis is scaled for each region"
  ) +
  ggplot2::geom_area(
    mapping = ggplot2::aes(
      x = BIN,
      y = n,
      fill = climatezone
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
    tidyr::drop_na(region, climatezone) %>%
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
      ncol = 1,
      labeller = ggplot2::labeller( 
        region = ggplot2::label_wrap_gen(7)
      )
    ) +
    ggplot2::scale_color_manual(
      values = palette_ecozones
    ) +
    ggplot2::scale_fill_manual(
      values = palette_ecozones
    ) +
    ggplot2::scale_x_continuous(
      trans = "reverse",
      limits = c(12.5e3, 0),
      breaks = seq(12.5e3, 0, by = -1e3),
      labels = seq(12.5, 0, by = -1)
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
        size = text_size, # [config criteria]
        color = common_gray, # [config criteria]
        hjust = 0.01
      ),
      panel.grid.minor = ggplot2::element_blank(),
        legend.text = ggplot2::element_text(
          size = text_size, # [config criteria]
          color = common_gray # [config criteria]
        ),
        legend.title = ggplot2::element_text(
          size = text_size, # [config criteria]
          color = common_gray # [config criteria]
        ),
        text = ggplot2::element_text(
          size = text_size, # [config criteria]
          color = common_gray # [config criteria]
        ),
        axis.text.y = ggplot2::element_text(
          size = text_size, # [config criteria]
          color = common_gray # [config criteria]
        ),
        axis.title.y = ggplot2::element_text(
          size = text_size, # [config criteria]
          color = common_gray # [config criteria]
        ),
        line = ggplot2::element_line(
          linewidth = line_size, # [config criteria]
          color = common_gray # [config criteria]
        )
      
    ) +
    ggplot2::geom_density(
      mapping = ggplot2::aes(
        x = age,
        y = ggplot2::after_stat(count),
        fill = climatezone,
        col = climatezone
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
  dplyr::filter(
    region != "Africa"
  ) %>%
  add_climatezone_as_factor() %>% # [config criteria]
  add_region_as_factor() %>% # [config criteria]
  dplyr::mutate(
    has_rc = purrr::map_lgl(
      .x = rc,
      .f = ~ is.data.frame(.x)
    )
  ) %>%
  dplyr::mutate(
    n_rc_250 = purrr::map2_dbl(
      .x = has_rc,
      .y = rc,
      .f = ~ ifelse(
        .x,
        .y %>%
          dplyr::filter(dist <= 250) %>%
          nrow(),
        0
      )
    ),
    n_rc_500 = purrr::map2_dbl(
      .x = has_rc,
      .y = rc,
      .f = ~ ifelse(
        .x,
        .y %>%
          dplyr::filter(dist <= 500) %>%
          nrow(),
        0
      )
    ),
    n_rc = purrr::map2_dbl(
      .x = has_rc,
      .y = rc,
      .f = ~ ifelse(.x, nrow(.y), 0)
    )
  ) %>%
  dplyr::mutate(
    has_valid_n_rc_250 = purrr::map_lgl(
      .x = n_rc_250,
      .f = ~ .x >= min_number_of_rc_dates, # [config]
    ),
    has_valid_n_rc_500 = purrr::map_lgl(
      .x = n_rc_500,
      .f = ~ .x >= min_number_of_rc_dates, # [config]
    ),
    has_valid_n_rc = purrr::map_lgl(
      .x = n_rc,
      .f = ~ .x >= min_number_of_rc_dates, # [config]
    )
  ) %>%
  dplyr::distinct(
    region, climatezone, dataset_id,
    has_valid_n_rc_250, has_valid_n_rc_500, has_valid_n_rc
  ) %>%
  tidyr::drop_na(region, climatezone)

# 250 km
data_valid_n_rc_250 <-
  data_valid_n_rc_raw %>%
  tidyr::drop_na(region, climatezone) %>%
  dplyr::group_by(region, climatezone, has_valid_n_rc_250) %>%
  dplyr::count(
    name = "N"
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(N = as.double(N)) %>%
  dplyr::arrange(
    region, climatezone, has_valid_n_rc_250
  ) %>%
  tidyr::complete(
    region,
    climatezone,
    has_valid_n_rc_250,
    fill = list(N = 0.00001)
  ) %>%
  tidyr::drop_na(region, climatezone)

fig_valid_n_rc_250 <-
  data_valid_n_rc_250 %>%
  ggplot2::ggplot() +
  ggplot2::facet_grid(
    region ~ climatezone,
    labeller = ggplot2::labeller( 
      region = ggplot2::label_wrap_gen(7),
      climatezone = ggplot2::label_wrap_gen(7) 
    ) 
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.title = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank(),
    legend.position = "right",
    plot.caption.position = "panel",
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = common_gray, # [config criteria]
      hjust = 0.01
    ),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank(),  
    legend.text = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = common_gray # [config criteria]
    ),
    legend.title = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = common_gray # [config criteria]
    ),
    text = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = common_gray # [config criteria]
    ),
    axis.text.y = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = common_gray # [config criteria]
    ),
    axis.title.y = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = common_gray # [config criteria]
    ),
    line = ggplot2::element_line(
      linewidth = line_size, # [config criteria]
      color = common_gray # [config criteria]
    )
  ) +
  ggplot2::labs(
    fill = "Has enough RC dates?",
    caption = "250 km is used as maximum distance from a record"
  ) +
  ggplot2::coord_equal() +
  waffle::geom_waffle(
    mapping = ggplot2::aes(
      fill = has_valid_n_rc_250,
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
      here::here("Outputs/Supp/C14_valid_datasets_250km"),
      .x,
      sep = "."
    ),
    plot = fig_valid_n_rc_250,
    width = image_width_vec["3col"], # [config criteria]
    height = 180,
    units = image_units, # [config criteria]
    bg = "white"
  )
)

# 500 km
data_valid_n_rc_500 <-
  data_valid_n_rc_raw %>%
  tidyr::drop_na(region, climatezone) %>%
  dplyr::group_by(region, climatezone, has_valid_n_rc_500) %>%
  dplyr::count(
    name = "N"
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(N = as.double(N)) %>%
  dplyr::arrange(
    region, climatezone, has_valid_n_rc_500
  ) %>%
  tidyr::complete(
    region,
    climatezone,
    has_valid_n_rc_500,
    fill = list(N = 0.00001)
  ) %>%
  tidyr::drop_na(region, climatezone)

fig_valid_n_rc_500 <-
  data_valid_n_rc_500 %>%
  ggplot2::ggplot() +
  ggplot2::facet_grid(
    region ~ climatezone,
    labeller = ggplot2::labeller( 
      region = ggplot2::label_wrap_gen(7),
      climatezone = ggplot2::label_wrap_gen(7) 
    ) 
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.title = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank(),
    legend.position = "right",
    plot.caption.position = "panel",
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = common_gray, # [config criteria]
      hjust = 0.01
    ),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = common_gray # [config criteria]
    ),
    legend.title = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = common_gray # [config criteria]
    ),
    text = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = common_gray # [config criteria]
    ),
    axis.text.y = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = common_gray # [config criteria]
    ),
    axis.title.y = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = common_gray # [config criteria]
    ),
    line = ggplot2::element_line(
      linewidth = line_size, # [config criteria]
      color = common_gray # [config criteria]
    )
  ) +
  ggplot2::labs(
    fill = "Has enough RC dates?",
    caption = "500 km is used as maximum distance from a record"
  ) +
  ggplot2::coord_equal() +
  waffle::geom_waffle(
    mapping = ggplot2::aes(
      fill = has_valid_n_rc_500,
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
      here::here("Outputs/Supp/C14_valid_datasets_500km"),
      .x,
      sep = "."
    ),
    plot = fig_valid_n_rc_500,
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
  ) %>%
  add_climatezone_as_factor() %>%
  add_region_as_factor()

data_valid_events <-
  data_valid_events_raw %>%
  dplyr::group_by(region, climatezone, have_events) %>%
  dplyr::count(
    name = "N"
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(N = as.double(N)) %>%
  dplyr::arrange(
    region, climatezone, have_events
  ) %>%
  tidyr::complete(
    region,
    climatezone,
    have_events,
    fill = list(N = 0.00001)
  ) %>%
  tidyr::drop_na(region, climatezone)

fig_human_presence_detected <-
  data_valid_events %>%
  ggplot2::ggplot() +
  ggplot2::facet_grid(
    region ~ climatezone,
    labeller = ggplot2::labeller( 
      region = ggplot2::label_wrap_gen(7),
      climatezone = ggplot2::label_wrap_gen(7) 
    ) 
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.title = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank(),
    legend.position = "right",
    plot.caption.position = "panel",
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(
      size = text_size,
      hjust = 0.01
    ),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = common_gray # [config criteria]
    ),
    legend.title = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = common_gray # [config criteria]
    ),
    text = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = common_gray # [config criteria]
    ),
    axis.text.y = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = common_gray # [config criteria]
    ),
    axis.title.y = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = common_gray # [config criteria]
    ),
    line = ggplot2::element_line(
      linewidth = line_size, # [config criteria]
      color = common_gray # [config criteria]
    )
  ) +
  ggplot2::labs(
    fill = "Human presence detected"
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
    by = dplyr::join_by(region, climatezone, dataset_id)
  ) %>%
  dplyr::mutate(
    status = dplyr::case_when(
      have_events == TRUE & has_valid_n_rc_500 == TRUE ~
        "human presence & enough RC",
      have_events == TRUE & has_valid_n_rc_500 == FALSE ~
        "human presence but not enough RC",
      have_events == FALSE & has_valid_n_rc_500 == TRUE ~
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
  dplyr::group_by(region, climatezone, status) %>%
  dplyr::count(
    name = "N"
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(N = as.double(N)) %>%
  dplyr::arrange(
    region, climatezone, status
  ) %>%
  tidyr::complete(
    region,
    climatezone,
    status,
    fill = list(N = 0.00001)
  ) %>%
  tidyr::drop_na(region, climatezone)   %>%
  ggplot2::ggplot() +
  ggplot2::facet_grid(
    region ~ climatezone,
    labeller = ggplot2::labeller( 
      region = ggplot2::label_wrap_gen(7),
      climatezone = ggplot2::label_wrap_gen(7) 
    ) 
  ) +
  ggplot2::theme_bw() +
  ggplot2::guides(
    fill = ggplot2::guide_legend(ncol = 1)
  ) +
  ggplot2::theme(
    axis.title = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank(),
    legend.position = "right",
    plot.caption.position = "panel",
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(
      size = text_size,
      color = common_gray,
      hjust = 0.01
    ),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = common_gray # [config criteria]
    ),
    legend.title = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = common_gray # [config criteria]
    ),
    text = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = common_gray # [config criteria]
    ),
    axis.title.y = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = common_gray # [config criteria]
    ),
    line = ggplot2::element_line(
      linewidth = line_size, # [config criteria]
      color = common_gray # [config criteria]
    )
  ) +
  ggplot2::labs(
    caption = paste(
      "Human presence is detected from pollen data.", "\n",
      "Enough RC dates are defined as >= 50.", "\n",
      "250/500 km is used as maximum distance from a record."
    ),
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

extended_figure_2 <-
  cowplot::plot_grid(
    fig_n_c14,
    fig_human_presence_status,
    nrow = 2
  )

purrr::walk(
  .x = c("png", "pdf"),
  .f = ~ ggplot2::ggsave(
    paste(
      here::here("Outputs/Supp/Extended_data_figure_2"),
      .x,
      sep = "."
    ),
    plot = extended_figure_2,
    width = image_width_vec["3col"], # [config criteria]
    height = 250,
    units = image_units, # [config criteria]
    bg = "white"
  )
)

#----------------------------------------------------------#
# 6. Extended data figure 3: 
#    Temporal trends of human impact  -----
#----------------------------------------------------------#

mod_config_file <-
  RUtilpol::get_latest_file(
    file_name = "predictor_models_config_table",
    dir = paste0(
      data_storage_path,
      "Data/Predictor_models/"
    )
  )

data_general_tredns_events_raw <-
  get_all_predicted_general_trends(
    data_source = mod_config_file,
    sel_type = "events"
  )

data_general_tredns_constant <-
  RUtilpol::get_latest_file(
    file_name = "predictor_models_data_constant",
    dir = paste0(
      data_storage_path,
      "Data/Predictor_models/"
    )
  )

data_general_tredns_constant  %>% 
dplyr::distinct(region, climatezone, variable) 

data_general_tredns_events <-
  dplyr::bind_rows(
    data_general_tredns_constant,
    data_general_tredns_events_raw
  ) %>%
  reorder_region_and_climate_zone() %>%
  dplyr::select(
    dplyr::all_of(
      c(
        "region",
        "climatezone",
        "variable",
        "age",
        "value"
      )
    )
  ) %>%
  dplyr::mutate(
    variable = dplyr::case_when(
      .default = "no impact",
      variable == "bi" ~ "no impact",
      variable == "fi" ~ "first impact",
      variable == "ei" ~ "emerging impact",
      variable == "ec" ~ "extensive clearance",
      variable == "cc" ~ "complete clearance",
      variable == "fc" ~ "first cultivation",
      variable == "es" ~ "european settlement",
      variable == "weak" ~ "weak impact",
      variable == "medium" ~ "medium impact",
      variable == "strong" ~ "strong impact"
    ),
    variable = factor(
      variable,
      levels = c(
        "no impact",
        "first impact",
        "emerging impact",
        "extensive clearance",
        "complete clearance",
        "first cultivation",
        "european settlement",
        "weak impact",
        "medium impact",
        "strong impact"
      )
    )
  ) %>%
  add_climatezone_as_factor() %>%
  add_region_as_factor()

event_color_palette <-
  c(
    "grey60",
    "#c99000",
    "#a17400",
    "#7b5800",
    "#573e00",
    "#00c92b",
    "#c9009e",
    "#9b541b",
    "#5d261a",
    "#1f0000"
  ) %>%
  rlang::set_names(
    levels(data_general_tredns_events$variable)
  )

fig_event_temporal_trends <-
  data_general_tredns_events %>%
  ggplot2::ggplot(
    mapping = ggplot2::aes(
      x = age,
      y = value,
      col = variable
    )
  ) +
  ggplot2::facet_grid(
    region ~ climatezone,
    labeller = ggplot2::labeller( 
      region = ggplot2::label_wrap_gen(7),
      climatezone = ggplot2::label_wrap_gen(7) 
    ) 
    ) +
  ggplot2::scale_x_continuous(
    trans = "reverse",
    limits = c(12e3, 0),
    breaks = seq(12e3, 0, by = -2e3),
    labels = seq(12, 0, by = -2)
  ) +
  ggplot2::scale_fill_manual(
    values = event_color_palette
  ) +
  ggplot2::scale_color_manual(
    values = event_color_palette
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "bottom",
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(
      size = text_size,
      color = common_gray, # [config criteria]
      hjust = 0.01
    ),
    legend.text = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = common_gray # [config criteria]
    ),
    text = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = common_gray # [config criteria]
    ),
    line = ggplot2::element_line(
      linewidth = line_size, # [config criteria]
      color = common_gray # [config criteria]
    ),
    panel.grid.minor = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank()
  ) +
  ggplot2::labs(
    x = "Age (cal ka BP)"
  ) +
  ggplot2::geom_ribbon(
    mapping = ggplot2::aes(
      ymin = 0,
      ymax = value,
      fill = variable
    ),
    alpha = 0.3
  )


purrr::walk(
  .x = c("png", "pdf"),
  .f = ~ ggplot2::ggsave(
    paste(
      here::here("Outputs/Supp/Extended_data_figure_3"),
      .x,
      sep = "."
    ),
    plot = fig_event_temporal_trends,
    width = image_width_vec["3col"], # [config criteria]
    height = 180,
    units = image_units, # [config criteria]
    bg = "white"
  )
)
