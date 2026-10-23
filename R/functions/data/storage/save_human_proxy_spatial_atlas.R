#' @title Save a matched human-proxy spatial atlas
#' @description
#' Export one PNG per age and a single multi-page landscape PDF containing all
#' atlas pages in descending age order.
#' @param plot_pages Named list of ggplot-compatible atlas pages. Names must be
#'   numeric ages in cal yr BP.
#' @param output_directory Output directory created when absent.
#' @param basename Stable semantic basename for the multi-page PDF.
#' @param width_mm Page width in millimetres.
#' @param height_mm Page height in millimetres.
#' @param dpi PNG resolution.
#' @return Character vector of all written file paths.
#' @examples
#' \dontrun{
#' save_human_proxy_spatial_atlas(pages, tempdir())
#' }
save_human_proxy_spatial_atlas <- function(
  plot_pages,
  output_directory,
  basename = "matched_human_proxies__spatial_atlas__world_and_europe__2_to_8ka",
  width_mm = 297,
  height_mm = 210,
  dpi = 300
) {
  assertthat::assert_that(
    is.list(plot_pages), length(plot_pages) > 0L,
    !is.null(names(plot_pages)), all(nzchar(names(plot_pages))),
    all(is.finite(suppressWarnings(as.numeric(names(plot_pages))))),
    is.character(output_directory), length(output_directory) == 1L,
    is.character(basename), length(basename) == 1L, nzchar(basename),
    is.numeric(width_mm), length(width_mm) == 1L, width_mm > 0,
    is.numeric(height_mm), length(height_mm) == 1L, height_mm > 0,
    is.numeric(dpi), length(dpi) == 1L, dpi > 0,
    msg = "Human-proxy spatial-atlas export inputs are invalid."
  )
  dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

  ages <- suppressWarnings(as.numeric(names(plot_pages)))
  plot_pages <- plot_pages[order(ages, decreasing = TRUE)]
  ages <- suppressWarnings(as.numeric(names(plot_pages)))
  png_paths <- file.path(
    output_directory,
    sprintf("matched_human_proxies__world_and_europe__%05d_cal_bp.png", ages)
  )
  purrr::walk2(plot_pages, png_paths, .f = ~ {
    ggplot2::ggsave(
      filename = .y,
      plot = .x,
      width = width_mm,
      height = height_mm,
      units = "mm",
      dpi = dpi,
      bg = "white"
    )
  })

  pdf_path <- file.path(output_directory, paste0(basename, ".pdf"))
  grDevices::pdf(
    file = pdf_path,
    width = width_mm / 25.4,
    height = height_mm / 25.4,
    onefile = TRUE,
    useDingbats = FALSE
  )
  on.exit(grDevices::dev.off(), add = TRUE)
  purrr::walk(plot_pages, print)
  grDevices::dev.off()
  on.exit(NULL, add = FALSE)

  res <- c(pdf_path, png_paths)

  return(res)
}
