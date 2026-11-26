#' @title Compute one overall dbMEM-adjusted balance from design matrices
#' @description
#' Fit the weighted aggregation model by least squares and return the
#' weighted mean of its predictions with the spatial terms set to zero.
#' @param mat_design
#' Numeric model matrix of the aggregation model (stratum indicators and
#' selected dbMEMs).
#' @param mat_design_adjusted
#' `mat_design` with every dbMEM column set to zero.
#' @param vec_response
#' Numeric response, one value per row.
#' @param vec_weight
#' Positive numeric weights, one per row.
#' @return
#' A numeric scalar: the weighted mean of the adjusted predictions over rows
#' with a finite prediction and a positive weight, or `NA` when none remains
#' or the fit fails.
#' @details
#' The model is fitted with `stats::lm.wfit()`. Coefficients aliased by rank
#' deficiency are treated as zero, as `stats::predict.lm()` does for a
#' rank-deficient fit.
#' @examples
#' compute_adjusted_balance_estimate(
#'   mat_design = cbind(a = c(1, 1, 0, 0), b = c(0, 0, 1, 1)),
#'   mat_design_adjusted = cbind(a = c(1, 1, 0, 0), b = c(0, 0, 1, 1)),
#'   vec_response = c(0.1, 0.3, -0.2, 0),
#'   vec_weight = c(1, 1, 1, 1)
#' )
compute_adjusted_balance_estimate <- function(
  mat_design,
  mat_design_adjusted,
  vec_response,
  vec_weight
) {
  assertthat::assert_that(
    is.matrix(mat_design),
    is.numeric(mat_design),
    is.matrix(mat_design_adjusted),
    identical(dim(mat_design), dim(mat_design_adjusted)),
    is.numeric(vec_response),
    length(vec_response) == nrow(mat_design),
    is.numeric(vec_weight),
    length(vec_weight) == nrow(mat_design),
    msg = "Adjusted-balance inputs do not satisfy the contract."
  )

  fit_attempt <-
    purrr::safely(stats::lm.wfit)(
      x = mat_design,
      y = vec_response,
      w = vec_weight
    )

  if (
    !is.null(fit_attempt[["error"]])
  ) {
    return(NA_real_)
  }

  vec_coefficients <-
    fit_attempt[["result"]][["coefficients"]] |>
    tidyr::replace_na(replace = 0)

  vec_predicted <-
    as.numeric(mat_design_adjusted %*% vec_coefficients)

  vec_keep <-
    is.finite(vec_predicted) & is.finite(vec_weight) & vec_weight > 0

  res <-
    if (
      !any(vec_keep)
    ) {
      NA_real_
    } else {
      stats::weighted.mean(vec_predicted[vec_keep], vec_weight[vec_keep])
    }

  return(res)
}
