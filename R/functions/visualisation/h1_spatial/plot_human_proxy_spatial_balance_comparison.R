#' @title Plot a selected human-proxy spatial balance
#' @description Reuse the canonical Figure-3 spatial map, density, and
#' climate-zone layout for the single locally filtered joint-human model.
#' @param data_records Eligible dataset-level spatial balance records.
#' @param data_aggregations Spatial aggregation result for the joint model.
#' @param data_geo_koppen Köppen spatial layer.
#' @param model_specifications The one-row joint-model specification.
#' @return The canonical spatial balance figure without extra annotations.
#' @examples
#' \dontrun{plot_human_proxy_spatial_balance_comparison(records, fits, koppen, specs)}
plot_human_proxy_spatial_balance_comparison <- function(
  data_records,
  data_aggregations,
  data_geo_koppen,
  model_specifications
) {
  assertthat::assert_that(
    is.data.frame(data_records), is.data.frame(data_aggregations),
    nrow(model_specifications) == 1L,
    all(c("model_id", "result") %in% names(data_aggregations)),
    msg = "Filtered joint spatial-plot inputs do not satisfy the contract."
  )
  model_id <- model_specifications$model_id[[1]]
  fit_index <- match(model_id, data_aggregations$model_id)
  assertthat::assert_that(
    !is.na(fit_index),
    msg = "The filtered joint spatial aggregation is unavailable."
  )
  fit <- data_aggregations$result[[fit_index]]
  res <- plot_h1_spatial_controlled_balance(
    data_records = data_records |>
      dplyr::filter(.data[["model_id"]] == .env[["model_id"]]),
    data_estimates = fit$estimates,
    data_geo_koppen = data_geo_koppen,
    profile = "zero_truncated"
  )

  return(res)
}
