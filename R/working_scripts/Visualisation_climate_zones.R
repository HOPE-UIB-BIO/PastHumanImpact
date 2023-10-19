#----------------------------------------------------------#
#
#
#                   GlobalHumanImpact
#
#                    VISUALISATION
#      Supplementary: Spatial coverage of climate zones
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

fig_climate_zone <-
  data_geo_koppen %>%
  ggplot2::ggplot() +
  ggplot2::borders(
    fill = "grey90",
    colour = "grey90",
  ) +
  ggplot2::geom_raster(
    mapping = ggplot2::aes(
      x = x,
      y = y,
      fill = sel_classification
    ),
    alpha = 1
  ) +
  ggplot2::scale_fill_manual(
    values = palette_ecozones,
    drop = FALSE
  ) +
  ggplot2::coord_sf(
    expand = TRUE
  ) +
  ggplot2::theme_void() +
  ggplot2::theme(
    legend.position = "none",
    panel.background = ggplot2::element_blank(),
    plot.background = ggplot2::element_blank(),
    panel.spacing = ggplot2::unit(c(0, 0, 0, 0), "null"),
    plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm"),
  )

purrr::walk(
  .x = c("png", "pdf"),
  .f = ~ ggplot2::ggsave(
    paste(
      here::here("Outputs/Supp/Climate_zones_map"),
      .x,
      sep = "."
    ),
    plot = fig_climate_zone,
    width = image_width_vec["3col"], # [config criteria]
    height = 130,
    units = image_units, # [config criteria]
    bg = "white"
  )
)
