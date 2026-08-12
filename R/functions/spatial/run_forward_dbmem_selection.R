#' @title Run forward selection for residual dbMEM predictors
#' @description
#' Run Blanchet double-stopping forward selection and return a tibble while
#' suppressing console output produced by `adespatial::forward.sel()`.
#' @param response_residual Residual response matrix.
#' @param mem_residual Residual candidate dbMEM matrix.
#' @param max_selected Maximum number of terms that may be selected.
#' @param adjusted_r_squared Maximum cumulative adjusted R-squared.
#' @param permutations Number of permutations.
#' @param alpha Significance threshold.
#' @return A tibble containing the forward-selection path.
#' @examples
#' \dontrun{
#' run_forward_dbmem_selection(
#'   response_residual = response,
#'   mem_residual = mem,
#'   max_selected = 3L,
#'   adjusted_r_squared = 0.2,
#'   permutations = 99L,
#'   alpha = 0.05
#' )
#' }
run_forward_dbmem_selection <- function(
  response_residual,
  mem_residual,
  max_selected,
  adjusted_r_squared,
  permutations,
  alpha
) {
  assertthat::assert_that(
    is.matrix(response_residual),
    is.matrix(mem_residual),
    nrow(response_residual) == nrow(mem_residual),
    is.numeric(max_selected),
    length(max_selected) == 1L,
    max_selected >= 1L,
    is.numeric(adjusted_r_squared),
    length(adjusted_r_squared) == 1L,
    is.finite(adjusted_r_squared),
    is.numeric(permutations),
    length(permutations) == 1L,
    permutations >= 1L,
    is.numeric(alpha),
    length(alpha) == 1L,
    dplyr::between(alpha, 0, 1),
    msg = "Forward dbMEM selection inputs do not satisfy the contract."
  )

  result_selection <-
    purrr::safely(
      .f = purrr::quietly(adespatial::forward.sel)
    )(
      Y = response_residual,
      X = mem_residual,
      K = max_selected,
      R2thresh = 1,
      adjR2thresh = adjusted_r_squared,
      nperm = permutations,
      alpha = alpha,
      verbose = FALSE
    )

  if (
    !is.null(result_selection[["error"]])
  ) {
    error_message <- conditionMessage(result_selection[["error"]])

    if (
      stringr::str_detect(error_message, "No variables selected")
    ) {
      return(tibble::tibble())
    }

    stop(result_selection[["error"]])
  }

  res_selection <-
    result_selection[["result"]][["result"]] |>
    tibble::as_tibble()

  return(res_selection)
}
