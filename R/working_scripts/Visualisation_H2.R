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
# 4. proportion of explaoned eigenvalues plot -----
#----------------------------------------------------------#

data_proportion_variance <-
  output_hvar_h2 %>%
  dplyr::mutate(
    summary_variation = purrr::map(
      .x = varhp,
      .f = ~ .x %>%
        purrr::pluck("summary_variation")
    )
  ) %>%
  dplyr::select(-c(data_merge, responce_dist, varhp)) %>%
  tidyr::unnest(summary_variation) %>%
  dplyr::rename(
    sel_classification = group
  ) %>%
  janitor::clean_names()


data_fig_variance <-
  tibble::tibble(
    region = vec_regions # [config criteria]
  ) %>%
  dplyr::mutate(
    plot = purrr::map(
      .x = region,
      .f = ~ {
        data_sel <-
          data_proportion_variance %>%
          dplyr::filter(region == .x) %>%
          dplyr::select(-region) %>%
          dplyr::mutate(sel_classification = as.factor(sel_classification)) %>%
          dplyr::full_join(
            data_climate_zones, # [config criteria]
            .,
            by = "sel_classification"
          )

        fig_basic <-
          data_sel %>%
          ggplot2::ggplot(
            mapping = ggplot2::aes(
              x = 1,
              y = (constrained_eig / total_eig) * 100
            )
          ) +
          ggplot2::facet_wrap(~sel_classification, nrow = 1) +
          ggplot2::scale_y_continuous(
            limits = c(0, 100)
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
            legend.position = "none",
            panel.spacing.x = grid::unit(0, "mm"),
            panel.border = ggplot2::element_blank(),
            strip.background = ggplot2::element_blank(),
            strip.text = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank(),
            axis.title = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            panel.grid.major.x = ggplot2::element_blank(),
            plot.margin = grid::unit(c(0.1, 0.1, 0.1, 0.1), "mm")
          )

        fig_basic +
          ggplot2::geom_segment(
            mapping = ggplot2::aes(
              xend = 1,
              yend = 0
            ),
            col = "grey30"
          ) +
          ggplot2::geom_point(
            mapping = ggplot2::aes(
              fill = sel_classification
            ),
            shape = 21,
            col = "gray30",
            size = 3
          )
      }
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

#----------------------------------------------------------#
# 5. temporal m2 -----
#----------------------------------------------------------#

data_m2_change_region <-
  data_m2 %>%
  dplyr::select(
    m2_time_df,
    region,
    sel_classification
  ) %>%
  dplyr::filter(!region == "Africa") %>%
  dplyr::inner_join(
    data_meta %>%
      dplyr::select(region, sel_classification, ecozone_koppen_5) %>%
      dplyr::distinct(),
    by = c("region", "sel_classification")
  ) %>%
  tidyr::unnest(cols = c(m2_time_df)) %>%
  dplyr::group_by(region, ecozone_koppen_5) %>%
  tidyr::nest(data_to_plot = -c(region, ecozone_koppen_5)) %>%
  dplyr::mutate(
    name = paste0(region, "_", ecozone_koppen_5)
  ) %>%
  dplyr::mutate(
    plot = purrr::map(
      .x = data_to_plot,
      .f = ~ ggplot2::ggplot(
        data = .x,
        ggplot2::aes(
          x = as.numeric(time),
          y = delta_m2,
          col = sel_classification,
          fill = sel_classification
        )
      ) +
        ggplot2::geom_point(size = 0.5) +
        ggplot2::geom_smooth(linewidth = 0.1, se = FALSE) +
        ggplot2::scale_x_continuous(
          limits = c(500, 8500),
          breaks = c(seq(500, 8500, by = 2000))
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
          panel.grid.minor = ggplot2::element_blank(),
          plot.background = ggplot2::element_rect(
            fill = "transparent",
            color = NA
          ),
          panel.grid.major = ggplot2::element_line(
            color = "grey90",
            linewidth = 0.1
          ),
          axis.title.y = ggplot2::element_text(size = 6),
          axis.text.x = ggplot2::element_text(size = 6, angle = 60),
          axis.text.y = ggplot2::element_text(size = 6),
          plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm")
        ) +
        ggplot2::labs(
          x = "",
          y = "change in m2"
        )
    )
  ) %>%
  dplyr::ungroup()

data_m2_change_region_wider <-
  data_m2_change_region %>%
  dplyr::select(region, ecozone_koppen_5, plot) %>%
  tidyr::pivot_wider(
    names_from = ecozone_koppen_5,
    values_from = plot
  )

#----------------------------------------------------------#
# 7. Combine plots -----
#----------------------------------------------------------#

fig_grid_maps <-
  cowplot::plot_grid(
    plotlist = list_circulal_plots_on_maps,
    nrow = 5,
    ncol = 1
  )

fig_grid_model <-
  cowplot::plot_grid(
    plotlist = data_fig_variance$plot,
    nrow = 5,
    ncol = 1,
    align = "v",
    axis = "t"
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
