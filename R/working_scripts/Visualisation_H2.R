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
        rasterdata = data_geo_koppen,
        select_region = select_region,
        sel_palette = get_greyout_palette(.x)
      )
    ) %>%
    return()
}

get_curve_with_insert <- function(data_source = data_m2_change_region_wider,
                                  sel_region,
                                  sel_climate,
                                  remove = "NULL") {
  sel_curve <-
    data_source %>%
    dplyr::filter(region == sel_region) %>%
    purrr::chuck(sel_climate, 1)

  switch(remove,
    "x" = {
      sel_curve <-
        sel_curve +
        ggplot2::theme(
          axis.title.x = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank()
        )
    },
    "y" = {
      sel_curve <-
        sel_curve +
        ggplot2::theme(
          axis.title.y = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank()
        )
    },
    "both" = {
      sel_curve <-
        sel_curve +
        ggplot2::theme(
          axis.title.x = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank()
        )
    }
  )

  res <-
    sel_curve +
    patchwork::inset_element(
      list_region_maps_climate %>%
        purrr::chuck(sel_region, sel_climate),
      left = 0.365, bottom = 0.7, right = 1, top = 1
    )

  return(res)
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

list_region_maps_grey <-
  vec_regions %>% # [config criteria]
  purrr::map(
    .f = ~ get_map_region(
      rasterdata = data_geo_koppen,
      select_region = .x,
      sel_palette = palette_ecozones, # [config criteria]
      sel_alpha = 0
    )
  )

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
        data = .x,
        y_var = "percentage",
        fill_var = "group",
        x_var = "predictor",
        line_width = 0.2,
        line_col = "grey75",
        col_vec = palette_ecozones, # [config criteria]
        x_name = predictors_label # [config criteria]
      )
    )
  )

list_circulal_plots_on_maps <-
  vec_regions %>% # [config criteria]
  purrr::map(
    .progress = TRUE,
    .f = ~ cowplot::ggdraw(
      list_region_maps_grey[[.x]]
    ) +
      cowplot::draw_plot(
        get_plot_by_region(
          data_circular_bar_h2,
          sel_region = .x
        )
      )
  )

fig_grid_maps <-
  cowplot::plot_grid(
    plotlist = list_circulal_plots_on_maps,
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



#----------------------------------------------------------#
# 7. Combine plots -----
#----------------------------------------------------------#

fig_m2_change_region +
  patchwork::inset_element(
    list_region_maps_climate %>%
      purrr::chuck("Oceania", "Tropical"),
    left = 0.72, right = 0.80,
    bottom = 0.1, top = 0.18
  ) +
  patchwork::inset_element(
    list_region_maps_climate %>%
      purrr::chuck("Latin America", "Tropical"),
    left = 0.72, right = 0.80,
    bottom = 0.7, top = 0.78
  ) +
    patchwork::inset_element(
      list_region_maps_climate %>%
        purrr::chuck("Latin America", "Arid"),
      left = 0.47, right = 0.55,
      bottom = 0.7, top = 0.78
    )

# //TODO find a way to do this programatically
fig_grid_curve <-
  cowplot::plot_grid(
    get_curve_with_insert(
      sel_region = "North America", sel_climate = "Polar",
      remove = "x"
    ),
    get_curve_with_insert(
      sel_region = "North America", sel_climate = "Cold",
      remove = "both"
    ),
    get_curve_with_insert(
      sel_region = "North America", sel_climate = "Temperate",
      remove = "both"
    ),
    get_curve_with_insert(
      sel_region = "North America", sel_climate = "Arid",
      remove = "both"
    ),
    get_curve_with_insert(
      sel_region = "North America", sel_climate = "Tropical",
      remove = "NULL"
    ),
    get_curve_with_insert(
      sel_region = "Latin America", sel_climate = "Polar",
      remove = "x"
    ),
    get_curve_with_insert(
      sel_region = "Latin America", sel_climate = "Cold",
      remove = "NULL"
    ),
    get_curve_with_insert(
      sel_region = "Latin America", sel_climate = "Temperate",
      remove = "both"
    ),
    get_curve_with_insert(
      sel_region = "Latin America", sel_climate = "Arid",
      remove = "both"
    ),
    get_curve_with_insert(
      sel_region = "Latin America", sel_climate = "Tropical",
      remove = "both"
    ),
    get_curve_with_insert(
      sel_region = "Europe", sel_climate = "Polar",
      remove = "x"
    ),
    get_curve_with_insert(
      sel_region = "Europe", sel_climate = "Cold",
      remove = "both"
    ),
    get_curve_with_insert(
      sel_region = "Europe", sel_climate = "Temperate",
      remove = "both"
    ),
    get_curve_with_insert(
      sel_region = "Europe", sel_climate = "Arid",
      remove = "both"
    ),
    get_curve_with_insert(
      sel_region = "Europe", sel_climate = "Tropical",
      remove = "NULL"
    ),
    get_curve_with_insert(
      sel_region = "Asia", sel_climate = "Polar"
    ),
    get_curve_with_insert(
      sel_region = "Asia", sel_climate = "Cold",
      remove = "y"
    ),
    get_curve_with_insert(
      sel_region = "Asia", sel_climate = "Temperate",
      remove = "both"
    ),
    get_curve_with_insert(
      sel_region = "Asia", sel_climate = "Arid",
      remove = "y"
    ),
    get_curve_with_insert(
      sel_region = "Asia", sel_climate = "Tropical",
      remove = "NULL"
    ),
    get_curve_with_insert(
      sel_region = "Oceania", sel_climate = "Polar"
    ),
    get_curve_with_insert(
      sel_region = "Oceania", sel_climate = "Cold",
      remove = ""
    ),
    get_curve_with_insert(
      sel_region = "Oceania", sel_climate = "Temperate"
    ),
    get_curve_with_insert(
      sel_region = "Oceania", sel_climate = "Arid",
      remove = ""
    ),
    get_curve_with_insert(
      sel_region = "Oceania", sel_climate = "Tropical",
      remove = "y"
    ),
    nrow = 5,
    ncol = 5,
    align = "hv"
  )

combine_h2 <-
  ggpubr::ggarrange(
    fig_grid_maps,
    fig_grid_model,
    fig_grid_curve,
    nrow = 1,
    ncol = 3,
    widths = c(1, 0.8, 5)
  )

purrr::walk(
  .x = c("png", "pdf"),
  .f = ~ ggplot2::ggsave(
    paste(
      here::here("Outputs/combine_h2"),
      .x,
      sep = "."
    ),
    plot = combine_h2,
    width = image_width_vec["3col"], # [config criteria]
    height = 200,
    units = image_units, # [config criteria]
    bg = "white"
  )
)
