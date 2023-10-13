#----------------------------------------------------------#
#
#
#                   GlobalHumanImpact
#
#                    VISUALISATION
#                 IGURE 3: RESULTS H2
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
list_maps <- function(select_region) {
  # helper function
  get_greyout_palette <- function(sel_climate_zone,
                                  default_palette = palette_ecozones # [config criteria]
  ) {
    default_palette[
      stringr::str_detect(
        names(default_palette), sel_climate_zone,
        negate = TRUE
      )
    ] <- "grey80"

    return(default_palette)
  }

  vec_climate_5 %>% # [config criteria]
    rlang::set_names() %>%
    purrr::map(
      .f = ~ get_map_region(
        rasterdata = data_geo_koppen %>%
          dplyr::filter(region == select_region),
        select_region = select_region,
        sel_palette = get_greyout_palette(.x)
      )
    ) %>%
    return()
}

add_map_to_facet <- function(
    data_source, sel_region, sel_climate) {
  ## This function allows us to specify which facet to annotate
  annotation_custom2 <- function(grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data) {
    ggplot2::layer(
      data = data,
      stat = ggplot2::StatIdentity,
      position = ggplot2::PositionIdentity,
      geom = ggplot2:::GeomCustomAnn,
      inherit.aes = TRUE,
      params = list(
        grob = grob,
        xmin = xmin, xmax = xmax,
        ymin = ymin, ymax = ymax
      )
    )
  }

  annotation_custom2(
    grob = ggplot2::ggplotGrob(
      data_source %>%
        purrr::chuck(sel_region, sel_climate)
    ),
    data = data.frame(
      region = sel_region,
      ecozone_koppen_5 = sel_climate,
      sel_classification = sel_climate,
      time = 0,
      delta_m2 = 0
    ),
    xmin = -4500, xmax = -500,
    ymin = 0.4, ymax = 1
  )
}

#----------------------------------------------------------#
# 1. Load data -----
#----------------------------------------------------------#

data_for_hvar <-
  targets::tar_read(
    name = "data_for_hvar",
    store = paste0(
      data_storage_path,
      "_targets_h1"
    )
  )

data_for_m2 <-
  data_for_hvar %>%
  dplyr::inner_join(
    data_meta %>%
      dplyr::select(
        dataset_id,
        region,
        sel_classification
      ),
    by = "dataset_id"
  )

# Import data for mapping
data_geo_koppen <-
  readr::read_rds(
    paste0(
      data_storage_path,
      "Data/ecoregions2017/data_geo_koppen.rds"
    )
  ) %>%
  tibble::as_tibble() %>%
  # add new classification
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
# 2. rerun m2 with sel_classification -----
#----------------------------------------------------------#
# temporarily quick fix
data_m2 <-
  get_data_m2(
    data_source = data_for_hvar,
    data_meta = data_meta,
    min_samples = 5,
    select_vars = c(
      "dataset_id",
      "age",
      "region",
      "sel_classification",
      "n0",
      "n1",
      "n2",
      "n1_minus_n2",
      "n2_divided_by_n1",
      "n1_divided_by_n0",
      "dcca_axis_1", "roc",
      "density_turnover",
      "density_diversity"
    )
  )

#----------------------------------------------------------#
# 3. make maps -----
#----------------------------------------------------------#

list_region_maps_climate <-
  vec_regions %>% # [config criteria]
  purrr::map(
    .f = ~ list_maps(
      select_region = .x
    )
  )

#----------------------------------------------------------#
# 6. circular plot -----
#----------------------------------------------------------#

data_circular_bar_h2 <-
  data_h2_vis %>%
  dplyr::mutate(group = factor(group)) %>%
  dplyr::group_by(region) %>%
  tidyr::nest(data_to_plot = -c(region)) %>%
  dplyr::mutate(
    plot = purrr::map(
      .x = data_to_plot,
      .f = ~ get_circular_barplot(
        data_source = .x,
        y_var = "percentage",
        fill_var = "group",
        x_var = "predictor",
        line_width = 0.2,
        line_col = "grey75",
        icon_size = 0.15,
        y_max = 100,
        y_step = 30,
        col_vec = palette_ecozones, # [config criteria]
        x_name = predictors_label # [config criteria]
      )
    )
  )

list_circulal_plots <-
  vec_regions %>% # [config criteria]
  purrr::map(
    .progress = TRUE,
    .f = ~ get_plot_by_region(
      data_circular_bar_h2,
      sel_region = .x
    )
  )

fig_grid_maps <-
  cowplot::plot_grid(
    plotlist = list_circulal_plots,
    nrow = 5,
    ncol = 1
  )


#----------------------------------------------------------#
# 5. temporal m2 -----
#----------------------------------------------------------#

fig_m2_change_region <-
  data_m2 %>%
  dplyr::select(
    m2_time_df,
    region,
    sel_classification
  ) %>%
  dplyr::filter(!region == "Africa") %>%
  tidyr::unnest(cols = c(m2_time_df)) %>%
  tidyr::complete(
    time,
    tidyr::nesting(sel_classification, region)
  ) %>%
  dplyr::inner_join(
    data_meta %>%
      dplyr::select(region, sel_classification, ecozone_koppen_5) %>%
      dplyr::distinct(),
    by = c("region", "sel_classification")
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
  ggplot2::ggplot(
    ggplot2::aes(
      x = as.numeric(time),
      y = delta_m2,
      col = sel_classification,
      fill = sel_classification
    )
  ) +
  ggplot2::facet_grid(
    region ~ ecozone_koppen_5
  ) +
  ggplot2::scale_x_continuous(
    trans = "reverse",
    limits = c(8.5e3, 500),
    breaks = c(seq(8.5e3, 500, by = -2e3)),
    labels = c(seq(8.5, 0.5, by = -2))
  ) +
  ggplot2::scale_y_continuous(limits = c(0, 1)) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_manual(
    values = palette_ecozones,
    drop = FALSE
  ) +
  ggplot2::scale_fill_manual(
    values = palette_ecozones,
    drop = FALSE
  ) +
  ggplot2::theme(
    aspect.ratio = 1,
    legend.position = "none",
    panel.background = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank(),
    strip.text.y = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    plot.background = ggplot2::element_rect(
      fill = "transparent",
      color = NA
    ),
    panel.grid.major = ggplot2::element_line(
      color = "grey90",
      linewidth = 0.1
    ),
    axis.title.x = ggplot2::element_text(size = 6),
    axis.title.y = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(size = 6, angle = 60),
    axis.text.y = ggplot2::element_text(size = 6),
    plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm")
  ) +
  ggplot2::labs(
    x = "Age (ka cal yr BP)",
    y = "change in m2"
  ) +
  ggplot2::geom_point(size = 0.5) +
  ggplot2::geom_smooth(
    method = "loess",
    formula = y ~ x,
    linewidth = 0.1,
    lty = 2,
    se = FALSE
  ) +
  ggplot2::geom_smooth(
    method = "gam",
    se = FALSE,
    formula = y ~ s(x, bs = "tp", k = 10),
    method.args = list(
      family =
        mgcv::betar(link = "logit")
    ),
    linewidth = 0.2
  )

data_region_climate_dummy <-
  tidyr::expand_grid(
    region = vec_regions, # [config criteria]
    ecozone_koppen_5 = vec_climate_5 # [config criteria]
  ) %>%
  dplyr::filter(!region == "Africa") %>%
  dplyr::mutate(
    region = factor(region,
      levels = vec_regions # [config criteria]
    ),
    ecozone_koppen_5 = factor(
      ecozone_koppen_5,
      levels = vec_climate_5 # [config criteria]
    )
  )

fig_grid_curve <-
  fig_m2_change_region +
  purrr::map2(
    .progress = TRUE,
    .x = data_region_climate_dummy$region,
    .y = data_region_climate_dummy$ecozone_koppen_5,
    .f = ~ add_map_to_facet(
      list_region_maps_climate,
      sel_region = .x,
      sel_climate = .y
    )
  )

#----------------------------------------------------------#
# 7. Combine plots -----
#----------------------------------------------------------#

combine_h2 <-
  ggpubr::ggarrange(
    fig_grid_curve,
    fig_grid_maps,
    nrow = 1,
    ncol = 2,
    widths = c(5, 1)
  )

combine_h2_with_headings <-
  cowplot::plot_grid(
    patchwork::plot_spacer(),
    combine_h2,
    ncol = 1,
    nrow = 2,
    rel_heights = c(1, 10)
  )

purrr::walk(
  .x = c("png", "pdf"),
  .f = ~ ggplot2::ggsave(
    paste(
      here::here("Outputs/combine_h2"),
      .x,
      sep = "."
    ),
    plot = combine_h2_with_headings,
    width = image_width_vec["2col"], # [config criteria]
    height = 165,
    units = image_units, # [config criteria]
    bg = "white"
  )
)
