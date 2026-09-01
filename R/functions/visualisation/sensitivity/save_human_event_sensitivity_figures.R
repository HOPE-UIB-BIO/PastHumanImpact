#' Save split human-event sensitivity figures
#'
#' @param plot_spatial Spatial comparison figure.
#' @param temporal_plots Named list returned by
#'   `plot_human_event_temporal_split_comparison()`.
#' @param path_spatial Spatial output stem without extension.
#' @param temporal_paths Named temporal output stems without extensions.
#'
#' @return Paths to six written PNG and PDF files.
#'
#' @export
save_human_event_sensitivity_figures <- function(
  plot_spatial,
  temporal_plots,
  path_spatial,
  temporal_paths
) {
  expected_temporal <- c(
    "profiles",
    "changes"
  )
  assertthat::assert_that(
    inherits(plot_spatial, c("ggplot", "patchwork")),
    is.list(temporal_plots),
    identical(names(temporal_plots), expected_temporal),
    all(purrr::map_lgl(
      temporal_plots,
      ~ inherits(.x, c("ggplot", "patchwork"))
    )),
    assertthat::is.string(path_spatial),
    is.character(temporal_paths),
    identical(names(temporal_paths), expected_temporal),
    msg = "Human-event figure export inputs do not satisfy the contract."
  )

  stems <- c(spatial = path_spatial, temporal_paths)
  plots <- c(list(spatial = plot_spatial), temporal_plots)
  purrr::walk(stems, ~ dir.create(
    dirname(.x),
    recursive = TRUE,
    showWarnings = FALSE
  ))
  paths <-
    tidyr::expand_grid(
      figure = names(stems),
      extension = c("png", "pdf")
    ) |>
    dplyr::mutate(
      path = stringr::str_c(stems[.data[["figure"]]], ".", .data[["extension"]]),
      height = dplyr::if_else(.data[["figure"]] == "spatial", 210, 190)
    )

  purrr::pwalk(
    paths,
    ~ ggplot2::ggsave(
      filename = ..3,
      plot = plots[[..1]],
      width = image_width_vec[["2col"]],
      height = ..4,
      units = image_units,
      dpi = 300,
      bg = "white"
    )
  )

  return(normalizePath(
    paths[["path"]],
    winslash = "/",
    mustWork = TRUE
  ))
}
