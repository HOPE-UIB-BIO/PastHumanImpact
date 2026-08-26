#' @title Save one dataset temporal figure
#' @description Build and save one dataset-level temporal diagnostic figure.
#' @param data_raw Pre-interpolation temporal values for one dataset.
#' @param data_observed Interpolated temporal observations for one dataset.
#' @param data_predictions Dataset-specific predictions for the same dataset.
#' @param data_metadata One-row metadata data frame for the dataset.
#' @param output_dir Existing figure output directory.
#' @param rewrite Logical. If `TRUE`, replace an existing figure.
#' @param width Numeric figure width in millimetres.
#' @param height Numeric figure height in millimetres.
#' @param dpi Numeric PNG resolution.
#' @param verbose Logical. If `TRUE`, report saved figures.
#' @return Character scalar containing the output path.
#' @examples
#' \dontrun{
#' path_figure <- save_dataset_temporal_figure(
#'   data_raw = data_dataset_raw,
#'   data_observed = data_dataset_observed,
#'   data_predictions = data_dataset_predictions,
#'   data_metadata = data_dataset_metadata,
#'   output_dir = "Outputs/Figures/Dataset_trends"
#' )
#' }
save_dataset_temporal_figure <- function(
  data_raw,
  data_observed,
  data_predictions,
  data_metadata,
  output_dir,
  rewrite = FALSE,
  width = 300,
  height = 160,
  dpi = 300,
  verbose = TRUE
) {
  assertthat::assert_that(
    is.data.frame(data_raw),
    is.data.frame(data_observed),
    is.data.frame(data_predictions),
    is.data.frame(data_metadata),
    "dataset_id" %in% names(data_raw),
    "dataset_id" %in% names(data_observed),
    "dataset_id" %in% names(data_predictions),
    nrow(data_raw) > 0L,
    nrow(data_observed) > 0L,
    nrow(data_predictions) > 0L,
    msg = "Core figure inputs must contain observations and predictions."
  )
  assertthat::assert_that(
    is.character(output_dir),
    length(output_dir) == 1L,
    dir.exists(output_dir),
    is.logical(rewrite),
    length(rewrite) == 1L,
    !is.na(rewrite),
    is.numeric(width),
    length(width) == 1L,
    width > 0,
    is.numeric(height),
    length(height) == 1L,
    height > 0,
    is.numeric(dpi),
    length(dpi) == 1L,
    dpi > 0,
    is.logical(verbose),
    length(verbose) == 1L,
    !is.na(verbose),
    msg = "Core figure output controls must be valid scalars."
  )

  dataset_id <-
    as.character(unique(data_observed[["dataset_id"]]))

  assertthat::assert_that(
    length(dataset_id) == 1L,
    grepl("^[A-Za-z0-9._-]+$", dataset_id),
    msg = "`dataset_id` must be a filename-safe scalar."
  )

  path_output <-
    file.path(
      output_dir,
      stringr::str_glue("dataset__{dataset_id}.png")
    )

  if (
    file.exists(path_output) && isFALSE(rewrite)
  ) {
    return(path_output)
  }

  plot_dataset <-
    plot_dataset_temporal_trends(
      data_raw = data_raw,
      data_observed = data_observed,
      data_predictions = data_predictions,
      data_metadata = data_metadata
    )

  ggplot2::ggsave(
    filename = path_output,
    plot = plot_dataset,
    width = width,
    height = height,
    units = "mm",
    dpi = dpi,
    bg = "white"
  )

  if (
    isTRUE(verbose)
  ) {
    cli::cli_inform("Saved dataset temporal figure {.file {path_output}}.")
  }

  return(path_output)
}
