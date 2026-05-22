#' @title A wrapper function to run Principal Coordinate Analysis on procrustes sum-of-square (m2) distances
#' @param pcoa The matrix of sum-of-squares (m2)
#' @return A list returned by `stats::cmdscale()`.

run_pcoa <- function(data_m2) {
  assertthat::assert_that(
    is.matrix(data_m2),
    msg = "`data_m2` must be a matrix."
  )
  assertthat::assert_that(
    nrow(data_m2) == ncol(data_m2),
    msg = "`data_m2` must be a square matrix."
  )
  assertthat::assert_that(
    !any(is.na(data_m2[lower.tri(data_m2)])),
    msg = "`data_m2` must not contain NA values in the lower triangle."
  )

  data_m2 <-
    stats::as.dist(data_m2)

  procrust_pcoa <-
    stats::cmdscale(data_m2,
      eig = TRUE,
      add = TRUE
    )
  return(procrust_pcoa)
}
