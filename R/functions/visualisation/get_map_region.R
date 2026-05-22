# function to get regional maps
# input is raster data for ecozones

#' @title Build a regional climate-zone map
#' @description
#' Creates a ggplot map for a selected region using rasterized climate-zone
#' data and configured regional boundaries.
#' @param rasterdata Data frame containing `x`, `y`, and `climatezone` columns.
#' @param select_region Region name used to subset configured map limits.
#' @param sel_alpha Alpha transparency for raster layer.
#' @param base_col Base color used for borders.
#' @param sel_palette Named palette for climate-zone fills.
#' @return A `ggplot` object.

get_map_region <- function(rasterdata,
                           select_region = "Europe",
                           sel_alpha = 1,
                           base_col = "grey90",
                           sel_palette = palette_ecozones # [config criteria]
) {
  assertthat::assert_that(
    is.data.frame(rasterdata),
    msg = "`rasterdata` must be a data frame."
  )
  assertthat::assert_that(
    all(c("x", "y", "climatezone") %in% names(rasterdata)),
    msg = "`rasterdata` must contain `x`, `y`, and `climatezone`."
  )
  assertthat::assert_that(
    is.character(select_region),
    msg = "`select_region` must be character."
  )
  assertthat::assert_that(
    is.numeric(sel_alpha) && length(sel_alpha) == 1,
    msg = "`sel_alpha` must be a single numeric value."
  )
  assertthat::assert_that(
    is.character(base_col) && length(base_col) == 1,
    msg = "`base_col` must be a single character value."
  )

  boundary <-
    data_regional_limits %>% # [config criteria]
    dplyr::filter(region %in% select_region)

  map_region <-
    rasterdata %>%
    ggplot2::ggplot() +
    ggplot2::borders(
      fill = base_col,
      colour = base_col,
    ) +
    ggplot2::geom_raster(
      mapping = ggplot2::aes(
        x = x,
        y = y,
        fill = climatezone
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
      legend.text = ggplot2::element_text(
        size = text_size, # [config criteria]
        colour = common_gray # [config criteria]
      ),
      legend.title = ggplot2::element_text(
        size = text_size, # [config criteria]
        colour = common_gray # [config criteria]
      ),
      panel.background = ggplot2::element_blank(),
      plot.background = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(c(0, 0, 0, 0), "null"),
      plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm"),
    )

  return(map_region)
}
