#' @title Combine HVarPart and temporal results for one core
#' @description
#' Place the HVarPart importance summary beside the raw, interpolated, and
#' fitted temporal trajectories for one dataset.
#' @param data_raw Pre-interpolation temporal values for one dataset.
#' @param data_observed Interpolated model observations for one dataset.
#' @param data_predictions Dataset-specific posterior predictions.
#' @param data_metadata One-row metadata data frame for the dataset.
#' @param data_importance Long-format HVarPart importance results.
#' @param climate_palette Named character vector of climate-zone colours.
#' @param predictor_palette Named character vector with human and climate
#' colours.
#' @return A combined `ggplot` object.
#' @examples
#' \dontrun{
#' plot_hvarpart_core_temporal_example(
#'   data_raw = data_core_raw,
#'   data_observed = data_core_observed,
#'   data_predictions = data_core_predictions,
#'   data_metadata = data_core_metadata,
#'   data_importance = data_hvarpart_importance
#' )
#' }
plot_hvarpart_core_temporal_example <- function(
  data_raw,
  data_observed,
  data_predictions,
  data_metadata,
  data_importance,
  climate_palette = palette_ecozones,
  predictor_palette = palette_predictors
) {
  assertthat::assert_that(
    is.data.frame(data_raw),
    is.data.frame(data_observed),
    is.data.frame(data_predictions),
    is.data.frame(data_metadata),
    is.data.frame(data_importance),
    "dataset_id" %in% names(data_observed),
    msg = "Core example inputs must be data frames with a dataset ID."
  )

  dataset_id <-
    as.character(unique(data_observed[["dataset_id"]]))

  assertthat::assert_that(
    length(dataset_id) == 1L,
    msg = "A core example must describe one dataset."
  )

  plot_importance <-
    plot_hvarpart_importance(
      data_importance = data_importance,
      dataset_id = dataset_id,
      predictor_palette = predictor_palette
    )
  plot_temporal <-
    plot_core_temporal_trends(
      data_raw = data_raw,
      data_observed = data_observed,
      data_predictions = data_predictions,
      data_metadata = data_metadata,
      climate_palette = climate_palette,
      predictor_palette = predictor_palette
    )
  res_plot <-
    cowplot::plot_grid(
      plot_importance,
      plot_temporal,
      nrow = 1,
      rel_widths = c(2.2, 14),
      align = "h",
      axis = "tb"
    )

  return(res_plot)
}
