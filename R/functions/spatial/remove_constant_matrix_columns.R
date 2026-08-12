#' @title Remove constant columns from a numeric matrix
#' @description
#' Retain columns with a finite, strictly positive standard deviation.
#' @param data_matrix Numeric matrix or data frame.
#' @return A numeric matrix containing only non-constant columns.
#' @examples
#' \dontrun{
#' remove_constant_matrix_columns(data_matrix = matrix(1:6, ncol = 2))
#' }
remove_constant_matrix_columns <- function(data_matrix) {
  assertthat::assert_that(
    is.matrix(data_matrix) || is.data.frame(data_matrix),
    msg = "`data_matrix` must be a matrix or data frame."
  )

  mat_values <- as.matrix(data_matrix)
  vec_keep <-
    seq_len(ncol(mat_values)) |>
    purrr::map_lgl(
      .f = ~ {
        value_sd <- stats::sd(mat_values[, .x])
        is.finite(value_sd) && value_sd > 0
      }
    )

  res_matrix <- mat_values[, vec_keep, drop = FALSE]

  return(res_matrix)
}
