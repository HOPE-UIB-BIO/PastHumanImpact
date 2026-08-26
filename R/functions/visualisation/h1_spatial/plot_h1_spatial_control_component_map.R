#' @title Plot one spatial structural-control component
#' @description
#' Plot one HVarPart component with its canonical semantic colour.
#' @param data_values Component values with coordinates.
#' @param component Component role shown in the facet strip.
#' @param component_colour Canonical colour for the component.
#' @param scale_type Either `"allocation"` or `"signed"`.
#' @param value_limits Numeric colour-scale limits.
#' @param legend_title Colour legend title.
#' @return A ggplot map.
#' @examples
#' \dontrun{
#' plot_h1_spatial_control_component_map(
#'   values,
#'   "human",
#'   palette_predictors[["human"]],
#'   "allocation",
#'   c(0, 1),
#'   "Contribution"
#' )
#' }
plot_h1_spatial_control_component_map <- function(
  data_values,
  component,
  component_colour,
  scale_type,
  value_limits,
  legend_title
) {
  assertthat::assert_that(
    is.data.frame(data_values),
    all(c("long", "lat", "value") %in% names(data_values)),
    length(component) == 1L,
    length(component_colour) == 1L,
    scale_type %in% c("allocation", "signed"),
    length(value_limits) == 2L,
    msg = "Spatial control component-map inputs are invalid."
  )

  data_values <-
    data_values |>
    dplyr::mutate(component = .env[["component"]])

  colour_scale <- if (
    identical(scale_type, "allocation")
  ) {
    ggplot2::scale_colour_gradient(
      low = "white",
      high = component_colour,
      limits = value_limits,
      oob = scales::squish
    )
  } else {
    ggplot2::scale_colour_gradient2(
      low = common_gray,
      mid = "white",
      high = component_colour,
      midpoint = 0,
      limits = value_limits,
      oob = scales::squish
    )
  }

  res <-
    ggplot2::ggplot() +
      ggplot2::geom_polygon(
        data = ggplot2::map_data("world"),
        ggplot2::aes(
          x = .data[["long"]],
          y = .data[["lat"]],
          group = .data[["group"]]
        ),
        fill = "grey95",
        colour = "grey75",
        linewidth = line_size
      ) +
      ggplot2::geom_point(
        data = data_values,
        ggplot2::aes(
          x = .data[["long"]],
          y = .data[["lat"]],
          colour = .data[["value"]]
        ),
        size = point_size,
        alpha = 0.7
      ) +
      colour_scale +
      ggplot2::coord_quickmap() +
      ggplot2::facet_wrap(
        ggplot2::vars(.data[["component"]]),
        ncol = 1
      ) +
      ggplot2::labs(colour = legend_title) +
      ggplot2::theme_void(base_size = text_size) +
      ggplot2::theme(legend.position = "bottom")

  return(res)
}
