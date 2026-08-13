#' @title Compute one temporal Moran diagnostic
#' @description
#' Calculate Moran's I and a one-sided ordered-permutation p-value at one
#' temporal distance threshold.
#' @param values Numeric response values ordered like the distance matrix.
#' @param distance_matrix Pairwise temporal distance matrix.
#' @param distance_years Positive temporal threshold.
#' @param permutation_matrix Integer permutation matrix with permutations in
#' rows.
#' @return A one-row temporal Moran diagnostic tibble.
#' @examples
#' \dontrun{
#' compute_temporal_moran_scale(
#'   values = 1:5,
#'   distance_matrix = compute_temporal_distance_matrix(seq(0, 2000, 500)),
#'   distance_years = 500,
#'   permutation_matrix = build_series_permutation_matrix(5, 9)
#' )
#' }
compute_temporal_moran_scale <- function(
  values,
  distance_matrix,
  distance_years,
  permutation_matrix
) {
  assertthat::assert_that(
    is.numeric(values),
    all(is.finite(values)),
    is.matrix(distance_matrix),
    nrow(distance_matrix) == length(values),
    ncol(distance_matrix) == length(values),
    is.numeric(distance_years),
    length(distance_years) == 1L,
    distance_years > 0,
    is.matrix(permutation_matrix),
    ncol(permutation_matrix) == length(values),
    msg = "Temporal Moran inputs do not satisfy the contract."
  )

  mat_weights <- 1 * (distance_matrix <= distance_years)
  diag(mat_weights) <- 0
  observed_i <-
    compute_moran_i(
      values = values,
      weights = mat_weights
    )
  vec_permuted_i <-
    seq_len(nrow(permutation_matrix)) |>
    purrr::map_dbl(
      .f = ~ compute_moran_i(
        values = values[permutation_matrix[.x, ]],
        weights = mat_weights
      )
    )
  expected_i <- -1 / (length(values) - 1)
  p_value <-
    if (
      is.finite(observed_i)
    ) {
      (1 + sum(vec_permuted_i >= observed_i, na.rm = TRUE)) /
        (nrow(permutation_matrix) + 1)
    } else {
      NA_real_
    }
  status <-
    dplyr::case_when(
      sum(mat_weights) == 0 ~ "no_links",
      !is.finite(observed_i) ~ "constant_response",
      .default = "estimated"
    )

  return(
    tibble::tibble(
      distance_years = distance_years,
      n_ages = length(values),
      n_connected = sum(rowSums(mat_weights) > 0),
      n_edges = sum(mat_weights) / 2,
      moran_i = observed_i,
      expected_i = expected_i,
      p_greater = p_value,
      positive_autocorrelation = is.finite(observed_i) &&
        observed_i > expected_i &&
        is.finite(p_value) &&
        p_value <= 0.05,
      permutations = nrow(permutation_matrix),
      status = status
    )
  )
}
