#' @title Function to extract a vector of consequtive time scores of sum-of-squares (m2) in the diagonal -1
#' @param data The matrix of all pairwise sum-of-squares

extract_m2_time <- function(data) {
  vec <- data[-1, ] %>% diag()
  names(vec) <- rownames(data[-1, ])
  vec
}
