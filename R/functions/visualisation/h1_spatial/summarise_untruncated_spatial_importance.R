#' @title Summarise untruncated spatial importance
#' @description Summarise the human HVarPart contribution for spatial groups.
#' @param data_importance Eligible predictor-level HVarPart results.
#' @param group_vars Character vector of grouping columns.
#' @param region_levels Ordered character vector of region names.
#' @return A grouped importance summary containing only the human predictor.
#' @examples
#' \dontrun{
#' summarise_untruncated_spatial_importance(
#'   data_importance = importance,
#'   group_vars = c("analysis", "region"),
#'   region_levels = c("Europe", "Asia")
#' )
#' }
summarise_untruncated_spatial_importance <- function(
  data_importance,
  group_vars,
  region_levels
) {
  assertthat::assert_that(
    is.data.frame(data_importance),
    is.character(group_vars),
    length(group_vars) > 0L,
    is.character(region_levels),
    length(region_levels) > 0L,
    msg = "Spatial importance summary inputs do not satisfy the contract."
  )

  res_summary <-
    summarise_hvarpart_importance(
      data_importance = data_importance,
      group_vars = group_vars,
      profile = "signed"
    ) |>
    dplyr::filter(.data[["predictor"]] == "human") |>
    dplyr::mutate(
      region = factor(.data[["region"]], levels = region_levels)
    )

  return(res_summary)
}
