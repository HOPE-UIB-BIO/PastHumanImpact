#' @title Compute residual RDA axes from an HVarPart design
#' @description Fit a complete RDA and return up to three residual site axes.
#' @param response Numeric response matrix.
#' @param predictors Numeric predictor matrix.
#' @param max_axes Maximum number of residual axes.
#' @return A tibble with residual-axis columns.
#' @examples
#' \dontrun{
#' compute_hvarpart_residual_axes(response_matrix, predictor_matrix)
#' }
compute_hvarpart_residual_axes <- function(
  response,
  predictors,
  max_axes = 3L
) {
  assertthat::assert_that(
    is.matrix(response),
    is.matrix(predictors),
    nrow(response) == nrow(predictors),
    nrow(response) >= 3L,
    is.numeric(max_axes),
    max_axes >= 1L,
    msg = "Residual-axis inputs do not satisfy the required contract."
  )

  mat_response <- scale(response, center = TRUE, scale = TRUE)
  mat_design <- cbind(intercept = 1, predictors)
  mat_residuals <-
    qr.resid(
      qr(mat_design),
      mat_response
    )

  if (
    ncol(mat_residuals) == 0L ||
      all(abs(mat_residuals) <= sqrt(.Machine[["double.eps"]]))
  ) {
    return(
      tibble::as_tibble(
        matrix(
          numeric(),
          nrow = nrow(response),
          ncol = 0L
        )
      )
    )
  }

  model_axes <- vegan::rda(X = mat_residuals, scale = TRUE)
  n_axes <-
    min(
      as.integer(max_axes),
      model_axes[["CA"]][["rank"]],
      ncol(mat_residuals),
      nrow(mat_residuals) - 1L
    )

  if (
    n_axes < 1L
  ) {
    return(
      tibble::as_tibble(
        matrix(
          numeric(),
          nrow = nrow(response),
          ncol = 0L
        )
      )
    )
  }

  mat_axes <-
    vegan::scores(
      x = model_axes,
      display = "sites",
      choices = seq_len(n_axes)
    )
  colnames(mat_axes) <-
    stringr::str_c("residual_axis_", seq_len(n_axes))

  return(tibble::as_tibble(mat_axes))
}
