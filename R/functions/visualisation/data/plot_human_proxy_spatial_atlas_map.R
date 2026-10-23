#' @title Plot one human-proxy spatial-atlas map
#' @description Draw one world or Europe panel using a fixed proxy-specific
#' colour scale.
#' @param data_proxy One age and proxy subset of spatial-atlas values.
#' @param world_map Polygon table with `long`, `lat`, and `group`.
#' @param view Either `world` or `europe`.
#' @param map_theme Theme shared by atlas panels.
#' @return A ggplot map object.
#' @examples
#' \dontrun{
#' plot_human_proxy_spatial_atlas_map(values, ggplot2::map_data("world"))
#' }
plot_human_proxy_spatial_atlas_map <- function(
  data_proxy,
  world_map,
  view = c("world", "europe"),
  map_theme = ggplot2::theme_void()
) {
  view <- match.arg(view)
  assertthat::assert_that(
    is.data.frame(data_proxy),
    all(c(
      "dataset_id", "proxy_label", "colour_value", "colour_max",
      "long", "lat", "region"
    ) %in% names(data_proxy)),
    is.data.frame(world_map),
    all(c("long", "lat", "group") %in% names(world_map)),
    inherits(map_theme, "theme"),
    msg = "Human-proxy atlas-map inputs do not satisfy the contract."
  )
  colour_max <- unique(data_proxy[["colour_max"]])
  if (length(colour_max) != 1L || !is.finite(colour_max)) {
    cli::cli_abort("Every proxy must have one finite fixed colour maximum.")
  }
  proxy_label <- as.character(unique(data_proxy[["proxy_label"]]))
  data_view <- if (view == "world") {
    data_proxy
  } else {
    data_proxy |>
      dplyr::filter(.data[["region"]] == "Europe")
  }
  limits <- if (view == "world") {
    list(x = c(-180, 180), y = c(-60, 85), title = "World")
  } else {
    list(x = c(-15, 45), y = c(32, 72), title = "Europe")
  }
  res <- ggplot2::ggplot() +
    ggplot2::geom_polygon(
      data = world_map,
      mapping = ggplot2::aes(
        x = .data[["long"]], y = .data[["lat"]],
        group = .data[["group"]]
      ),
      fill = "grey94", colour = "grey72", linewidth = 0.18
    ) +
    ggplot2::geom_point(
      data = data_view |>
        dplyr::arrange(.data[["colour_value"]]),
      mapping = ggplot2::aes(
        x = .data[["long"]], y = .data[["lat"]],
        colour = .data[["colour_value"]]
      ),
      size = if (view == "world") point_size * 1.4 else point_size * 1.8,
      alpha = 0.88
    ) +
    ggplot2::scale_colour_gradient(
      low = "#F5F1E5",
      high = palette_predictors[["human"]],
      limits = c(0, colour_max),
      oob = scales::squish,
      breaks = scales::breaks_pretty(n = 4),
      labels = scales::label_number(accuracy = NULL),
      guide = ggplot2::guide_colourbar(
        title = proxy_label,
        title.position = "top",
        barheight = grid::unit(20, "mm"),
        ticks.colour = common_gray,
        frame.colour = common_gray
      )
    ) +
    ggplot2::coord_quickmap(
      xlim = limits$x, ylim = limits$y, expand = FALSE, clip = "on"
    ) +
    ggplot2::labs(title = paste0(
      limits$title,
      " (n = ", dplyr::n_distinct(data_view[["dataset_id"]]), ")"
    )) +
    map_theme

  return(res)
}
