#' @title Save a PAP temporal figure
#' @description
#' Filter PAP plotting data, build a temporal figure, and save matching PNG and
#' PDF files.
#' @param data_observed PAP observations.
#' @param data_predictions PAP model predictions.
#' @param pap_variables Character vector of PAP variables to include.
#' @param layout Character scalar passed to `plot_pap_temporal_trends()`.
#' @param output_dir Existing output directory.
#' @param output_stem Filename without extension.
#' @param width Numeric figure width in millimetres.
#' @param height Numeric figure height in millimetres.
#' @param dpi Numeric PNG resolution.
#' @return The saved `ggplot` object.
#' @examples
#' \dontrun{
#' plot_pap <- save_pap_temporal_figure(
#'   data_observed = data_pap_observed,
#'   data_predictions = data_pap_predictions,
#'   pap_variables = "n0",
#'   layout = "strata",
#'   output_dir = paste0(
#'     "Outputs/Figures/Supplementary_figures/",
#'     "PAP_temporal_trends"
#'   ),
#'   output_stem = "PAP_through_time_n0",
#'   width = 270,
#'   height = 240
#' )
#' }
save_pap_temporal_figure <- function(
  data_observed,
  data_predictions,
  pap_variables,
  layout,
  output_dir,
  output_stem,
  width,
  height,
  dpi = 300
) {
  assertthat::assert_that(
    is.data.frame(data_observed),
    is.data.frame(data_predictions),
    is.character(pap_variables),
    length(pap_variables) > 0L,
    all(pap_variables %in% data_observed[["variable"]]),
    all(pap_variables %in% data_predictions[["variable"]]),
    msg = "PAP figure data must contain every requested variable."
  )
  assertthat::assert_that(
    is.character(output_dir),
    length(output_dir) == 1L,
    dir.exists(output_dir),
    is.character(output_stem),
    length(output_stem) == 1L,
    !is.na(output_stem),
    nzchar(output_stem),
    msg = "PAP figure output path must be valid."
  )
  assertthat::assert_that(
    is.numeric(width),
    length(width) == 1L,
    width > 0,
    is.numeric(height),
    length(height) == 1L,
    height > 0,
    is.numeric(dpi),
    length(dpi) == 1L,
    dpi > 0,
    msg = "PAP figure dimensions and resolution must be positive."
  )

  data_observed_selected <-
    data_observed %>%
    dplyr::filter(variable %in% pap_variables)
  data_predictions_selected <-
    data_predictions %>%
    dplyr::filter(variable %in% pap_variables)
  res_plot <-
    plot_pap_temporal_trends(
      data_observed = data_observed_selected,
      data_predictions = data_predictions_selected,
      layout = layout
    )

  purrr::walk(
    c("png", "pdf"),
    ~ ggplot2::ggsave(
      filename = file.path(
        output_dir,
        paste0(output_stem, ".", .x)
      ),
      plot = res_plot,
      width = width,
      height = height,
      units = "mm",
      dpi = dpi,
      bg = "white"
    )
  )

  return(res_plot)
}
