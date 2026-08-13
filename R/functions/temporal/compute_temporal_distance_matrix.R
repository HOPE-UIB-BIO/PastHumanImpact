#' @title Compute a temporal distance matrix
#' @description Calculate absolute pairwise distances between finite ages.
#' @param ages Numeric age vector.
#' @return A symmetric numeric matrix in the units of `ages`.
#' @examples
#' \dontrun{
#' compute_temporal_distance_matrix(c(0, 500, 1000))
#' }
compute_temporal_distance_matrix <- function(ages) {
  assertthat::assert_that(
    is.numeric(ages),
    length(ages) >= 2L,
    all(is.finite(ages)),
    !anyDuplicated(ages),
    msg = "Temporal distances require unique finite ages."
  )

  res_distances <- abs(outer(ages, ages, FUN = "-"))
  dimnames(res_distances) <- list(as.character(ages), as.character(ages))

  return(res_distances)
}
