#' @title Compute chi-square standardisation
#' @description
#' Apply the chi-square community transformation used by
#' `vegan::decostand(method = "chi.square")`.
#' @param data_source Numeric matrix or data frame with samples in rows.
#' @return A numeric matrix with the same dimensions and dimnames.
#' @examples
#' compute_chi_square_standardisation(matrix(c(1, 2, 3, 4), nrow = 2))
compute_chi_square_standardisation <- function(data_source) {
  assertthat::assert_that(
    is.matrix(data_source) || is.data.frame(data_source),
    msg = "`data_source` must be a matrix or data frame."
  )

  mat_source <-
    data.matrix(data_source)

  vec_row_totals <-
    rowSums(mat_source)

  vec_column_totals <-
    colSums(mat_source)

  assertthat::assert_that(
    all(is.finite(mat_source)),
    all(mat_source >= 0),
    all(vec_row_totals > 0),
    all(vec_column_totals > 0),
    msg = "Chi-square input must be finite with positive row and column sums."
  )

  mat_result <-
    sqrt(sum(mat_source)) * mat_source /
    outer(vec_row_totals, sqrt(vec_column_totals))

  return(mat_result)
}
