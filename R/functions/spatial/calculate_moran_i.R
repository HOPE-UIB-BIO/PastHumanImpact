#' @title Calculate Moran's I for one weight matrix
#' @description
#' Calculate Moran's I from a numeric response and a symmetric spatial-weight
#' matrix. Return `NA` when there are no links or the response is constant.
#' @param values Numeric response vector.
#' @param weights Numeric spatial-weight matrix.
#' @return One numeric Moran's I value or `NA_real_`.
#' @examples
#' \dontrun{
#' calculate_moran_i(values = c(1, 2), weights = matrix(c(0, 1, 1, 0), 2))
#' }
calculate_moran_i <- function(values, weights) {
  assertthat::assert_that(
    is.numeric(values),
    all(is.finite(values)),
    is.matrix(weights),
    is.numeric(weights),
    nrow(weights) == length(values),
    ncol(weights) == length(values),
    all(is.finite(weights)),
    msg = "Moran's I inputs do not satisfy the required contract."
  )

  vec_centered <- values - mean(values)
  weight_sum <- sum(weights)

  if (
    weight_sum == 0 || sum(vec_centered^2) == 0
  ) {
    return(NA_real_)
  }

  res_moran_i <-
    length(values) / weight_sum *
    sum(weights * tcrossprod(vec_centered)) /
    sum(vec_centered^2)

  return(res_moran_i)
}
