#' @title Function to extract a vector of consequtive time scores of sum-of-squares (m2) in the diagonal -1
#' @param data The matrix of all pairwise sum-of-squares
#' @return Named numeric vector of m2 values from the diagonal offset by one row.

extract_m2_time <- function(data) {
  assertthat::assert_that(
    is.matrix(data),
    msg = "`data` must be a matrix."
  )
  assertthat::assert_that(
    nrow(data) == ncol(data),
    msg = "`data` must be a square matrix."
  )

  vec <- data[-1, ] %>% diag()
  names(vec) <- rownames(data[-1, ])
  return(vec)
}
