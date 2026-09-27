#' @title Plot one age page of a matched human-proxy spatial atlas
#' @description
#' Draw paired world and Europe maps for square-root SPD, KK10 land-use
#' fraction, and square-root HYDE at the exact matched pollen-sequence
#' locations used by the sensitivity analysis.
#' @param data_values Long spatial-atlas values from
#'   `prepare_human_proxy_spatial_atlas_data()`.
#' @param age Single age in cal yr BP.
#' @param world_map Optional polygon table with `long`, `lat`, and `group`.
#' @return A ggplot-compatible cowplot object.
#' @examples
#' \dontrun{
#' plot_human_proxy_spatial_atlas_page(atlas$values, age = 8000)
#' }
plot_human_proxy_spatial_atlas_page <- function(
  data_values,
  age,
  world_map = ggplot2::map_data("world")
) {
  required_columns <- c(
    "dataset_id", "age", "proxy", "proxy_label", "value",
    "colour_value", "colour_max", "long", "lat", "region"
  )
  assertthat::assert_that(
    is.data.frame(data_values),
    all(required_columns %in% names(data_values)),
    is.numeric(age), length(age) == 1L, is.finite(age),
    is.data.frame(world_map),
    all(c("long", "lat", "group") %in% names(world_map)),
    msg = "Human-proxy spatial-atlas plot inputs do not satisfy the contract."
  )

  data_age <- data_values |>
    dplyr::filter(.data[["age"]] == .env[["age"]])
  proxy_levels <- c("spd_sqrt", "kk10_fraction", "hyde_sqrt")
  if (
    nrow(data_age) == 0L ||
      !setequal(as.character(unique(data_age[["proxy"]])), proxy_levels)
  ) {
    cli::cli_abort(
      "The requested atlas age must contain all three human proxies."
    )
  }

  map_theme <- ggplot2::theme_void(base_size = text_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = text_size * 0.9, face = "bold", colour = common_gray,
        margin = ggplot2::margin(0, 0, 1.5, 0, unit = "mm")
      ),
      legend.title = ggplot2::element_text(
        size = text_size * 0.68, colour = common_gray
      ),
      legend.text = ggplot2::element_text(
        size = text_size * 0.62, colour = common_gray
      ),
      legend.key.height = grid::unit(18, "mm"),
      legend.key.width = grid::unit(3.5, "mm"),
      plot.margin = ggplot2::margin(1, 2, 1, 2, unit = "mm")
    )

  proxy_rows <- purrr::map(proxy_levels, .f = ~ {
    proxy_name <- .x
    data_proxy <- data_age |>
      dplyr::filter(as.character(.data[["proxy"]]) == proxy_name)
    world_plot <- plot_human_proxy_spatial_atlas_map(
      data_proxy = data_proxy,
      world_map = world_map,
      view = "world",
      map_theme = map_theme
    )
    europe_plot <- plot_human_proxy_spatial_atlas_map(
      data_proxy = data_proxy,
      world_map = world_map,
      view = "europe",
      map_theme = map_theme
    )
    cowplot::plot_grid(
      world_plot + ggplot2::theme(legend.position = "none"),
      europe_plot + ggplot2::theme(legend.position = "right"),
      nrow = 1L,
      rel_widths = c(1.55, 1)
    )
  })

  title_grob <- cowplot::ggdraw() +
    cowplot::draw_label(
      paste0("Matched human-impact proxies - ", age / 1000, " cal ka BP"),
      fontface = "bold", size = text_size * 1.25,
      colour = common_gray, x = 0.5, hjust = 0.5
    )
  footer_grob <- cowplot::ggdraw() +
    cowplot::draw_label(
      paste(
        "Points are the exact matched H1 observations.",
        "Europe panels include Europe-assigned sequences only.",
        "Colour limits are fixed within proxy across all ages;",
        "values above the 99th percentile are saturated."
      ),
      size = text_size * 0.62, colour = common_gray,
      x = 0.01, hjust = 0
    )

  res <- cowplot::plot_grid(
    title_grob,
    cowplot::plot_grid(plotlist = proxy_rows, ncol = 1L),
    footer_grob,
    ncol = 1L,
    rel_heights = c(0.07, 1, 0.045)
  )

  return(res)
}
