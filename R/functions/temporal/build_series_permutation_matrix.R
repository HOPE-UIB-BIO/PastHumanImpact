#' @title Build ordered time-series permutations
#' @description
#' Generate reproducible cyclic-shift and mirror permutations while recording
#' the actual number available for a short time series.
#' @param n_values Number of ordered observations.
#' @param permutations Requested number of permutations.
#' @param seed Integer random seed.
#' @return An integer matrix with one permutation per row.
#' @examples
#' \dontrun{
#' build_series_permutation_matrix(10L, permutations = 99L)
#' }
build_series_permutation_matrix <- function(
  n_values,
  permutations = 999L,
  seed = 1234L
) {
  assertthat::assert_that(
    is.numeric(n_values),
    length(n_values) == 1L,
    n_values >= 3L,
    is.numeric(permutations),
    length(permutations) == 1L,
    permutations >= 1L,
    is.numeric(seed),
    length(seed) == 1L,
    msg = "Series permutation inputs do not satisfy the contract."
  )

  control <-
    permute::how(
      within = permute::Within(
        type = "series",
        mirror = TRUE
      )
    )
  res_permutations <-
    withr::with_seed(
      seed = seed,
      code = permute::shuffleSet(
        n = as.integer(n_values),
        nset = as.integer(permutations),
        control = control
      )
    )

  return(res_permutations)
}
