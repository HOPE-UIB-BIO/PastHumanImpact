#' @title Filter estimable region-by-age HVarPart compositions
#' @description
#' Keep the zero-truncated human, climate, and space compositions of
#' region-by-age units whose spatially controlled model was estimated and
#' whose three allocations sum to one.
#' @param data_composition
#' Output of `prepare_spatial_hvarpart_composition()`, with columns `region`,
#' `age`, `predictor`, `allocation`, and `status`.
#' @param estimated_statuses
#' Character vector of unit statuses that count as estimated.
#' @param tolerance
#' Largest accepted absolute deviation of an allocation sum from one.
#' @return
#' `data_composition` restricted to estimable units, with unchanged columns.
#' @details
#' A unit is kept when every row has an estimated status and a finite
#' allocation, it has exactly the three predictors `human`, `climate`, and
#' `space`, and its allocations sum to one within `tolerance`. This is the
#' rule of `prepare_human_proxy_comparison_tables()`.
#' @examples
#' \dontrun{
#' filter_estimable_spatial_compositions(compositions)
#' }
filter_estimable_spatial_compositions <- function(
  data_composition,
  estimated_statuses = c(
    "spatial_model_estimated",
    "no_spatial_terms_selected"
  ),
  tolerance = 1e-10
) {
  assertthat::assert_that(
    is.data.frame(data_composition),
    all(c("region", "age", "predictor", "allocation", "status") %in%
      names(data_composition)),
    is.character(estimated_statuses),
    is.numeric(tolerance),
    length(tolerance) == 1L,
    msg = "Composition inputs do not satisfy the estimable-unit contract."
  )

  data_valid_units <-
    data_composition |>
    dplyr::summarise(
      is_estimable = all(.data[["status"]] %in% estimated_statuses) &&
        all(is.finite(.data[["allocation"]])) &&
        setequal(.data[["predictor"]], c("human", "climate", "space")) &&
        dplyr::n() == 3L &&
        abs(sum(.data[["allocation"]]) - 1) < tolerance,
      .by = c("region", "age")
    ) |>
    dplyr::filter(.data[["is_estimable"]]) |>
    dplyr::select(dplyr::all_of(c("region", "age")))

  res <-
    data_composition |>
    dplyr::semi_join(data_valid_units, by = c("region", "age"))

  return(res)
}
