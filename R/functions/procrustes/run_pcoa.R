#' @title A wrapper function to run Principal Coordinate Analysis on procrustes sum-of-square (m2) distances
#' @param pcoa The matrix of sum-of-squares (m2)

run_pcoa <- function(data_m2) {
  data_m2 <-
    stats::as.dist(data_m2)

  procrust_pcoa <-
    stats::cmdscale(data_m2,
      eig = TRUE,
      add = TRUE
    )
  return(procrust_pcoa)
}
