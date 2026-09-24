#' @title Save joint human-proxy HVarPart figures
#' @description Save the spatial and temporal sensitivity figures as PNG/PDF.
#' @param plot_spatial Spatial human-climate balance figure.
#' @param plot_temporal Temporal human-climate-space composition figure.
#' @param path_spatial Output stem for the spatial figure.
#' @param path_temporal Output stem for the temporal figure.
#' @return Normalized paths to four written files.
#' @examples
#' \dontrun{
#' save_joint_human_proxy_hvarpart_figures(p1, p2, "spatial", "temporal")
#' }
save_joint_human_proxy_hvarpart_figures <- function(
  plot_spatial,
  plot_temporal,
  path_spatial,
  path_temporal
) {
  assertthat::assert_that(
    inherits(plot_spatial, "ggplot"),
    inherits(plot_temporal, "ggplot"),
    assertthat::is.string(path_spatial),
    assertthat::is.string(path_temporal),
    msg = "Joint human-proxy figure export inputs do not satisfy the contract."
  )

  figure_stems <- c(spatial = path_spatial, temporal = path_temporal)
  figure_plots <- list(spatial = plot_spatial, temporal = plot_temporal)
  figure_heights <- c(spatial = 130, temporal = 186.75)
  figure_widths <- c(
    spatial = image_width_vec[["2col"]],
    temporal = image_width_vec[["1col"]] * 1.5
  )

  purrr::walk(
    figure_stems,
    ~ dir.create(dirname(.x), recursive = TRUE, showWarnings = FALSE)
  )
  data_paths <-
    tidyr::expand_grid(
      figure = names(figure_stems),
      extension = c("png", "pdf")
    ) |>
    dplyr::mutate(
      path = stringr::str_c(
        figure_stems[.data[["figure"]]],
        ".",
        .data[["extension"]]
      )
    )

  purrr::pwalk(
    data_paths,
    function(figure, extension, path) {
      ggplot2::ggsave(
        filename = path,
        plot = figure_plots[[figure]],
        width = figure_widths[[figure]],
        height = figure_heights[[figure]],
        units = image_units,
        dpi = 300,
        bg = "white"
      )
    }
  )

  res_paths <- normalizePath(data_paths[["path"]], winslash = "/", mustWork = TRUE)

  return(res_paths)
}
