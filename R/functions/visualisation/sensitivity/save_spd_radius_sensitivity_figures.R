#' @title Save SPD-radius sensitivity figures
#' @description
#' Save the spatial comparison and separate temporal profile and change
#' figures as PNG and PDF.
#' @param plot_spatial Spatial comparison figure.
#' @param plot_temporal_profiles Temporal radius-profile figure.
#' @param plot_temporal_changes Temporal paired-change figure.
#' @param path_spatial Output stem without extension.
#' @param path_temporal_profiles Profile output stem without extension.
#' @param path_temporal_changes Change output stem without extension.
#' @return Paths to six written figure files.
#' @examples
#' \dontrun{
#' save_spd_radius_sensitivity_figures(
#'   p1, p2, p3, "spatial", "temporal-profiles", "temporal-changes"
#' )
#' }
save_spd_radius_sensitivity_figures <- function(
  plot_spatial,
  plot_temporal_profiles,
  plot_temporal_changes,
  path_spatial,
  path_temporal_profiles,
  path_temporal_changes
) {
  assertthat::assert_that(
    inherits(plot_spatial, c("ggplot", "patchwork")),
    inherits(plot_temporal_profiles, c("ggplot", "patchwork")),
    inherits(plot_temporal_changes, c("ggplot", "patchwork")),
    assertthat::is.string(path_spatial),
    assertthat::is.string(path_temporal_profiles),
    assertthat::is.string(path_temporal_changes),
    msg = "SPD radius figure export inputs do not satisfy the contract."
  )

  stems <-
    c(
      spatial = path_spatial,
      temporal_profiles = path_temporal_profiles,
      temporal_changes = path_temporal_changes
    )

  plots <-
    list(
      spatial = plot_spatial,
      temporal_profiles = plot_temporal_profiles,
      temporal_changes = plot_temporal_changes
    )

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
      path = stringr::str_c(stems[.data[["figure"]]], ".", .data[["extension"]])
    )

  purrr::pwalk(
    paths,
    ~ ggplot2::ggsave(
      filename = ..3,
      plot = plots[[..1]],
      width = image_width_vec[["2col"]],
      height = 210,
      units = image_units,
      dpi = 300,
      bg = "white"
    )
  )

  res <-
    normalizePath(paths[["path"]], winslash = "/", mustWork = TRUE)

  return(res)
}
