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
list_maps <- function(select_region) {
  # helper function
  get_greyout_palette <- function(
      sel_climate_zone,
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

  c(
    "Polar",
    "Cold",
    "Temperate",
    "Arid",
    "Tropical"
  ) %>%
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

vec_regions <- c(
  "North America",
  "Latin America",
  "Europe",
  "Asia",
  "Oceania"
) %>%
  rlang::set_names()


list_region_maps_grey <-
  vec_regions %>%
  purrr::map(
    .f = ~ get_map_region(
      rasterdata = data_geo_koppen,
      select_region = .x,
      sel_palette = palette_ecozones, # [config criteria]
      sel_alpha = 0
    )
  )

list_region_maps_climate <-
  vec_regions %>%
  purrr::map(
    .f = ~ list_maps(
      select_region = .x
    )
  )


#----------------------------------------------------------#
# 4. circular plot -----
#----------------------------------------------------------#
# GET FIGURES CIRCULAR BARS
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
        col_vec = palette_ecozones, # [config criteria]
        x_name = predictors_label # [config criteria]
      )
    )
  )

# GET FIGURES CONSECUTIVE PROCRUSTES CHANGE WITH TIME

m2_change_region <-
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
  tidzr::unnest(cols = c(m2_time_df)) %>%
  dplyr::group_by(region, ecozone_koppen_5) %>%
  dplyr::group_map(
    ~ ggplot2::ggplot(
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
      ggplot2::scale_x_reverse() +
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

# name list correct
groups <-
  data_m2 %>%
  dplyr::select(
    m2_time_df,
    region,
    sel_classification
  ) %>%
  dplyr::filter(!region %in% "Africa") %>%
  inner_join(
    data_meta %>%
      dplyr::select(region, sel_classification, ecozone_koppen_5) %>%
      dplyr::distinct(),
    by = c("region", "sel_classification")
  ) %>%
  tidyr::unnest(cols = c(m2_time_df)) %>%
  dplyr::group_by(region, ecozone_koppen_5) %>%
  attr("groups")

name_list <- paste0(groups$region, "_", groups$ecozone_koppen_5)

names(m2_change_region) <- name_list

# COMBINE FIGURES
# to shift themes
remove_axis_labels <-
  ggplot2::theme(
    axis.title.x = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank()
  )

remove_axis_labels_x <-
  ggplot2::theme(
    axis.title.x = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank()
  )

remove_axis_labels_y <-
  ggplot2::theme(
    axis.title.y = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank()
  )

remove_space <-
  ggplot2::theme(plot.margin = unit(c(0, 0, 0, 0), "mm"))

# PUT TOGETHER FIGURES PER REGION


dev.new(width = 180, height = 70, units = "mm")

design <- c(
  patchwork::area(t = 1, l = 1, b = 1, r = 1),
  patchwork::area(t = 1, l = 2, b = 1, r = 2),
  patchwork::area(t = 1, l = 3, b = 1, r = 3),
  patchwork::area(t = 1, l = 4, b = 1, r = 4),
  patchwork::area(t = 1, l = 5, b = 1, r = 5)
)


NA_p1 <-
  circular_bar_h2$`North America` +
  m2_change_region$`North America_Arid` +
  remove_axis_labels_x +
  remove_space +
  patchwork::inset_element(
    NA_map_list$arid,
    left = 0.365, bottom = 0.7, right = 1, top = 1
  ) +
  m2_change_region$`North America_Polar` +
  remove_axis_labels +
  remove_space +
  patchwork::inset_element(
    NA_map_list$polar,
    left = 0.365, bottom = 0.7, right = 1, top = 1
  ) +
  m2_change_region$`North America_Cold` +
  remove_axis_labels +
  remove_space +
  patchwork::inset_element(
    NA_map_list$cold,
    left = 0.365, bottom = 0.7, right = 1, top = 1
  ) +
  m2_change_region$`North America_Temperate` +
  remove_axis_labels +
  remove_space +
  patchwork::inset_element(
    NA_map_list$temp,
    left = 0.365, bottom = 0.7, right = 1, top = 1
  ) +
  patchwork::plot_layout(design = design)



LA_p1 <-
  circular_bar_h2$`Latin America` +
  m2_change_region$`Latin America_Arid` +
  remove_axis_labels_x +
  remove_space +
  patchwork::inset_element(
    LA_map_list$arid,
    left = 0.365, bottom = 0.7, right = 1, top = 0.9
  ) +
  m2_change_region$`Latin America_Polar` +
  remove_axis_labels +
  remove_space +
  patchwork::inset_element(
    LA_map_list$polar,
    left = 0.365, bottom = 0.7, right = 1, top = 0.9
  ) +
  m2_change_region$`Latin America_Temperate` +
  remove_axis_labels +
  remove_space +
  patchwork::inset_element(
    LA_map_list$temp,
    left = 0.365, bottom = 0.7, right = 1, top = 0.9
  ) +
  m2_change_region$`Latin America_Tropical` +
  remove_axis_labels +
  remove_space +
  patchwork::inset_element(
    LA_map_list$tropical,
    left = 0.365, bottom = 0.7, right = 1, top = 0.9
  ) +
  patchwork::plot_layout(design = design)


europe_p1 <-
  circular_bar_h2$`Europe` +
  m2_change_region$`Europe_Arid` +
  remove_axis_labels_x +
  remove_space +
  patchwork::inset_element(
    Europe_map_list$arid,
    left = 0.365, bottom = 0.7, right = 1, top = 1
  ) +
  m2_change_region$`Europe_Polar` +
  remove_axis_labels +
  remove_space +
  patchwork::inset_element(
    Europe_map_list$polar,
    left = 0.365, bottom = 0.7, right = 1, top = 1
  ) +
  m2_change_region$`Europe_Temperate` +
  remove_axis_labels +
  remove_space +
  patchwork::inset_element(
    Europe_map_list$temp,
    left = 0.365, bottom = 0.7, right = 1, top = 1
  ) +
  m2_change_region$`Europe_Cold` +
  remove_axis_labels +
  remove_space +
  patchwork::inset_element(
    Europe_map_list$cold,
    left = 0.365, bottom = 0.7, right = 1, top = 1
  ) +
  patchwork::plot_layout(design = design)




asia_p1 <-
  circular_bar_h2$`Asia` +
  m2_change_region$`Asia_Arid` +
  patchwork::inset_element(
    Asia_map_list$arid,
    left = 0.365, bottom = 0.7, right = 1, top = 1
  ) +
  remove_space +
  m2_change_region$`Asia_Polar` +
  remove_axis_labels_y +
  remove_space +
  patchwork::inset_element(
    Asia_map_list$polar,
    left = 0.365, bottom = 0.7, right = 1, top = 1
  ) +
  m2_change_region$`Asia_Temperate` +
  remove_axis_labels_y +
  remove_space +
  patchwork::inset_element(
    Asia_map_list$temp,
    left = 0.365, bottom = 0.7, right = 1, top = 1
  ) +
  m2_change_region$`Asia_Cold` +
  remove_axis_labels +
  remove_space +
  patchwork::inset_element(
    Asia_map_list$cold,
    left = 0.365, bottom = 0.7, right = 1, top = 1
  ) +
  patchwork::plot_layout(design = design)

o_design <- c(
  patchwork::area(t = 1, l = 1, b = 1, r = 1),
  patchwork::area(t = 1, l = 5, b = 1, r = 5)
)

oceania_p1 <-
  circular_bar_h2$`Oceania` +
  m2_change_region$`Oceania_Temperate` +
  remove_axis_labels_y +
  remove_space +
  patchwork::inset_element(
    oceania_map_list$temp,
    left = 0.365, bottom = 0.7, right = 1, top = 0.99
  ) +
  patchwork::plot_layout(design = o_design)


# FINAL COMBINED
design2 <- c(
  area(t = 1, l = 1, b = 1, r = 5),
  area(t = 2, l = 1, b = 2, r = 5),
  area(t = 3, l = 1, b = 3, r = 5),
  area(t = 4, l = 1, b = 4, r = 5),
  area(t = 5, l = 1, b = 5, r = 5)
)


# try design to keep each figure row at same plot level
combine_h2 <-
  NA_p1 /
  LA_p1 /
  europe_p1 /
  asia_p1 /
  oceania_p1 +
  patchwork::plot_layout(design = design2)

# try layout with ncol
combine_h2 <-
  NA_p1 /
  LA_p1 /
  europe_p1 /
  asia_p1 /
  oceania_p1 +
  patchwork::plot_layout(ncol = 1)


# try plot_grid from
combine_h2 <-
  cowplot::plot_grid(NA_p1,
    LA_p1,
    europe_p1,
    asia_p1,
    oceania_p1,
    ncol = 1
  )

combine_h2

dev.off()

# SAVE

# pdf("combined_h2_version3.pdf", width = 9, height = 9, unit = "cm")
# print(combine_h2)
# dev.off()
#
# system2('open', args = 'combined_h2_version3.pdf')

png("combined_plot_h2.png", width = 18, height = 9, unit = "cm", res = 300)
print(combine_h2)

dev.off()

system2("open", args = "combined_h2_version3.pdf")

ggplot2::ggsave(
  "combined_figure_h2.png",
  combine_h2,
  width = 18, height = 7.5, units = "cm",
  dpi = 500,
  bg = "white"
)
