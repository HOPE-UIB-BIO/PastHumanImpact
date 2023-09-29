# function to get regional maps
# input is raster data for ecozones

get_map_region <- function(rasterdata,
                           select_region = "Europe",
                           sel_alpha = 1,
                           sel_palette = palette_ecozones # [config criteria]
) {
  boundary <-
    data_regional_limits %>% # [config criteria]
    dplyr::filter(region %in% select_region)

  map_region <-
    rasterdata %>%
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
      alpha = sel_alpha
    ) +
    ggplot2::scale_fill_manual(
      values = sel_palette,
      drop = FALSE
    ) +
    ggplot2::coord_sf(
      expand = TRUE,
      ylim = c(boundary$ymin[1], boundary$ymax[1]),
      xlim = c(boundary$xmin[1], boundary$xmax[1])
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "none",
      panel.background = ggplot2::element_blank(),
      plot.background = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(c(0, 0, 0, 0), "null"),
      plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm"),
    )

  return(map_region)
}
