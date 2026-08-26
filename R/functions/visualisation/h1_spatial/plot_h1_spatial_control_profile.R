#' @title Plot a spatial structural-control profile
#' @description
#' Stack component maps while retaining the canonical colour for each role.
#' @param data_values Long component values with coordinates.
#' @param component_palette Named semantic component colours.
#' @param scale_type Either `"allocation"` or `"signed"`.
#' @param legend_title Colour legend title.
#' @return A vertically arranged ggplot object.
#' @examples
#' \dontrun{
#' plot_h1_spatial_control_profile(
#'   values,
#'   c(
#'     human = palette_predictors[["human"]],
#'     climate = palette_predictors[["climate"]],
#'     time = paletete_age[["old"]]
#'   ),
#'   "signed",
#'   "Unique adjusted R2"
#' )
#' }
plot_h1_spatial_control_profile <- function(
  data_values,
  component_palette,
  scale_type,
  legend_title
) {
  component_levels <- c("human", "climate", "time")
  assertthat::assert_that(
    is.data.frame(data_values),
    all(c("component", "long", "lat", "value") %in%
      names(data_values)),
    all(component_levels %in% names(component_palette)),
    scale_type %in% c("allocation", "signed"),
    msg = "Spatial control profile inputs are invalid."
  )

  value_limits <- if (
    identical(scale_type, "allocation")
  ) {
    c(0, 1)
  } else {
    range(c(data_values[["value"]], 0), finite = TRUE)
  }
  if (
    diff(value_limits) == 0
  ) {
    value_limits <- c(-0.1, 0.1)
  }

  plot_configuration <-
    data_values |>
    dplyr::mutate(
      component = factor(
        .data[["component"]],
        levels = component_levels
      )
    ) |>
    dplyr::filter(!is.na(.data[["component"]])) |>
    dplyr::arrange(.data[["component"]]) |>
    dplyr::group_by(.data[["component"]]) |>
    tidyr::nest() |>
    dplyr::ungroup() |>
    dplyr::mutate(
      component_colour = unname(
        component_palette[as.character(.data[["component"]])]
      ),
      scale_type = scale_type,
      value_limits = list(value_limits),
      legend_title = legend_title
    )

  component_maps <-
    plot_configuration |>
    dplyr::transmute(
      data_values = .data[["data"]],
      component = as.character(.data[["component"]]),
      component_colour = .data[["component_colour"]],
      scale_type = .data[["scale_type"]],
      value_limits = .data[["value_limits"]],
      legend_title = .data[["legend_title"]]
    ) |>
    purrr::pmap(plot_h1_spatial_control_component_map)

  result <-
    cowplot::plot_grid(
      plotlist = component_maps,
      ncol = 1,
      align = "v"
    )
  attr(result, "component_order") <-
    as.character(plot_configuration[["component"]])
  attr(result, "component_palette") <- component_palette[component_levels]

  return(result)
}
