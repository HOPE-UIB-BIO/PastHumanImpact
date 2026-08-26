#' @title Diagnose residual temporal autocorrelation
#' @description
#' Calculate ordered-permutation Moran diagnostics for numeric values observed
#' at unique ages and include the temporal connectivity threshold.
#' @param data_source Data frame containing age and value columns.
#' @param value_cols Numeric columns to diagnose.
#' @param distance_years Fixed temporal distance thresholds.
#' @param age_col Age column.
#' @param permutations Requested permutations.
#' @param seed Integer random seed.
#' @return One row per value and temporal threshold.
#' @examples
#' \dontrun{
#' compute_temporal_moran_diagnostics(
#'   data_source = residual_scores,
#'   value_cols = "residual_axis_1"
#' )
#' }
compute_temporal_moran_diagnostics <- function(
  data_source,
  value_cols,
  distance_years = c(500, 1000),
  age_col = "age",
  permutations = 999L,
  seed = 1234L
) {
  required_columns <- c(age_col, value_cols)
  assertthat::assert_that(
    is.data.frame(data_source),
    all(required_columns %in% names(data_source)),
    all(purrr::map_lgl(data_source[value_cols], is.numeric)),
    all(is.finite(unlist(data_source[required_columns]))),
    !anyDuplicated(data_source[[age_col]]),
    is.numeric(distance_years),
    all(distance_years > 0),
    msg = "Temporal diagnostic inputs do not satisfy the contract."
  )

  data_ordered <-
    data_source |>
    dplyr::arrange(.data[[age_col]])
  ages <- data_ordered[[age_col]]
  connectivity_threshold <- max(diff(ages))
  data_scales <-
    dplyr::bind_rows(
      tibble::tibble(
        temporal_scope =
          stringr::str_c("fixed_", distance_years, "y"),
        distance_years = distance_years
      ),
      tibble::tibble(
        temporal_scope = "connectivity",
        distance_years = connectivity_threshold
      )
    )
  mat_distances <- compute_temporal_distance_matrix(ages)
  mat_permutations <-
    build_series_permutation_matrix(
      n_values = length(ages),
      permutations = permutations,
      seed = seed
    )
  data_combinations <-
    tidyr::crossing(
      value = value_cols,
      data_scales
    )
  res_diagnostics <-
    data_combinations |>
    purrr::pmap(
      .f = ~ compute_temporal_moran_scale(
        values = data_ordered[[..1]],
        distance_matrix = mat_distances,
        distance_years = ..3,
        permutation_matrix = mat_permutations
      ) |>
        dplyr::mutate(
          value = ..1,
          temporal_scope = ..2,
          .before = 1L
        )
    ) |>
    dplyr::bind_rows()

  return(res_diagnostics)
}
