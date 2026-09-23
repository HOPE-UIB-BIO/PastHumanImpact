#' @title Save human-proxy convergence figures
#' @description Save overall and regional convergence figures as PNG and PDF.
#' @param plot_overall Overall comparison plot.
#' @param plot_regions Regional comparison plot.
#' @param path_overall Output stem for the overall figure.
#' @param path_regions Output stem for the regional figure.
#' @return Normalized paths to four written files.
#' @examples
#' \dontrun{
#' save_human_proxy_convergence_figures(p1, p2, "overall", "regions")
#' }
save_human_proxy_convergence_figures <- function(
  plot_overall,
  plot_regions,
  path_overall,
  path_regions
) {
  assertthat::assert_that(
    inherits(plot_overall, "ggplot"),
    inherits(plot_regions, "ggplot"),
    assertthat::is.string(path_overall),
    assertthat::is.string(path_regions),
    msg = "Human-proxy figure export inputs do not satisfy the contract."
  )

  vec_stems <-
    c(overall = path_overall, regions = path_regions)

  list_plots <-
    list(overall = plot_overall, regions = plot_regions)

  purrr::walk(
    vec_stems,
    ~ dir.create(dirname(.x), recursive = TRUE, showWarnings = FALSE)
  )

  data_paths <-
    tidyr::expand_grid(
      figure = names(vec_stems),
      extension = c("png", "pdf")
    ) |>
    dplyr::mutate(
      path = stringr::str_c(
        vec_stems[.data[["figure"]]],
        ".",
        .data[["extension"]]
      )
    )

  purrr::pwalk(
    data_paths,
    ~ ggplot2::ggsave(
      filename = ..3,
      plot = list_plots[[..1]],
      width = image_width_vec[["2col"]],
      height = 150,
      units = image_units,
      dpi = 300,
      bg = "white"
    )
  )

  res_paths <-
    normalizePath(data_paths[["path"]], winslash = "/", mustWork = TRUE)

  return(res_paths)
}
