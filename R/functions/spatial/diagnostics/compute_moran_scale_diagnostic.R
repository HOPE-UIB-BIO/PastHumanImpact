#' @title Compute one Moran diagnostic combination
#' @description
#' Calculate observed Moran's I and its blocked one-sided permutation test for
#' one response column and distance threshold.
#' @param data_source Spatial data frame containing the response.
#' @param value_col Name of the numeric response column.
#' @param distance_matrix Pairwise distance matrix in kilometres.
#' @param distance_km Distance threshold in kilometres.
#' @param blocks Permutation-block vector.
#' @param permutations Number of permutations.
#' @return A one-row Moran diagnostic tibble.
#' @examples
#' \dontrun{
#' compute_moran_scale_diagnostic(
#'   data_source = spatial_data,
#'   value_col = "response",
#'   distance_matrix = distances,
#'   distance_km = 250,
#'   blocks = spatial_data[["region"]],
#'   permutations = 99L
#' )
#' }
compute_moran_scale_diagnostic <- function(
  data_source,
  value_col,
  distance_matrix,
  distance_km,
  blocks,
  permutations
) {
  assertthat::assert_that(
    is.data.frame(data_source),
    assertthat::is.string(value_col),
    value_col %in% names(data_source),
    is.numeric(data_source[[value_col]]),
    is.matrix(distance_matrix),
    nrow(distance_matrix) == nrow(data_source),
    ncol(distance_matrix) == nrow(data_source),
    length(blocks) == nrow(data_source),
    is.numeric(distance_km),
    length(distance_km) == 1L,
    is.finite(distance_km),
    distance_km > 0,
    is.numeric(permutations),
    length(permutations) == 1L,
    permutations >= 1L,
    msg = "Single-scale Moran inputs do not satisfy the contract."
  )

  mat_weights <- 1 * (distance_matrix <= distance_km)
  diag(mat_weights) <- 0
  n_edges <- sum(mat_weights) / 2
  n_connected <- sum(rowSums(mat_weights) > 0)
  vec_values <- data_source[[value_col]]
  observed_i <-
    compute_moran_i(
      values = vec_values,
      weights = mat_weights
    )

  if (
    !is.finite(observed_i)
  ) {
    p_value <- NA_real_
    status <-
      if (
        n_edges == 0
      ) {
        "no_links"
      } else {
        "constant_response"
      }
  } else {
    mat_permuted <-
      build_block_permutation_matrix(
        values = vec_values,
        blocks = blocks,
        permutations = permutations
      )
    mat_centered <-
      sweep(
        x = mat_permuted,
        MARGIN = 2L,
        STATS = colMeans(mat_permuted),
        FUN = "-"
      )
    vec_permuted_i <-
      nrow(data_source) / sum(mat_weights) *
      colSums(mat_centered * (mat_weights %*% mat_centered)) /
      colSums(mat_centered^2)
    p_value <-
      (1 + sum(vec_permuted_i >= observed_i, na.rm = TRUE)) /
      (permutations + 1)
    status <- "estimated"
  }

  expected_i <- -1 / (nrow(data_source) - 1)
  res_diagnostic <-
    tibble::tibble(
      value = value_col,
      distance_km = distance_km,
      n_records = nrow(data_source),
      n_connected = n_connected,
      n_edges = n_edges,
      moran_i = observed_i,
      expected_i = expected_i,
      p_greater = p_value,
      positive_autocorrelation = is.finite(observed_i) &&
        observed_i > expected_i &&
        is.finite(p_value) && p_value <= 0.05,
      permutations = as.integer(permutations),
      status = status
    )

  return(res_diagnostic)
}
