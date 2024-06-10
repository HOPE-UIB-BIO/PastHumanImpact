#----------------------------------------------------------#
#
#
#                   GlobalHumanImpact
#
#                    VISUALISATION
#           Supplementary: make map of all records
#
#                   V. Felde, O. Mottl
#                         2024
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

# Load meta data
source(
  here::here(
    "R/main_analysis/02_meta_data.R"
  )
)

#----------------------------------------------------------#
# 1. Draw map -----
#----------------------------------------------------------#

get_map_worldmap_with_records <- function(data_source, point_size) {
  data_source %>%
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
      plot.margin = grid::unit(c(0.1, 0.1, 0.1, 0.1), "mm"),
      axis.line = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    ) +
    ggplot2::coord_equal(
      ratio = 1.3,
      ylim = range(data_meta$lat),
      xlim = range(data_meta$long)
    ) +
    ggplot2::labs(
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
      fill = common_gray, # [config criteria]
      alpha = 0.4
    ) +
    ggplot2::geom_point(
      mapping = ggplot2::aes(
        col = climatezone,
        fill = climatezone
      ),
      size = point_size,
      shape = 20,
      alpha = 1
    )
}

c(1:3) %>%
  purrr::walk(
    .f = ~ ggplot2::ggsave(
      plot = get_map_worldmap_with_records(data_meta, point_size = .x),
      filename = here::here(
        here::here(
          "Outputs/Figures/Extended_data_figures",
          "Map",
          paste0(
            "Map_of_all_records_pointsize_",
            .x,
            ".pdf"
          )
        )
      ),
      width = image_width_vec["2col"], # [config criteria]
      height = 80,
      units = image_units, # [config criteria]
      bg = "white"
    )
  )
