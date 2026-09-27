#' @title Build an empty temporal HVarPart result
#' @description Construct the standard temporal result contract for an
#' ineligible or failed local model.
#' @param status Classified model status.
#' @param n_samples Number of available observations.
#' @return A list matching the empty temporal HVarPart result contract.
#' @examples
#' build_empty_temporal_hvarpart_result("rank_deficient", 10L)
build_empty_temporal_hvarpart_result <- function(status, n_samples) {
  assertthat::assert_that(
    assertthat::is.string(status),
    is.numeric(n_samples), length(n_samples) == 1L, n_samples >= 0,
    msg = "Empty temporal HVarPart result inputs do not satisfy the contract."
  )
  res <- list(
    status = status,
    n_samples = as.integer(n_samples),
    n_unique_ages = NA_integer_,
    design_rank = NA_integer_,
    design_full_rank = NA,
    residual_df = NA_integer_,
    human_climate_only_hvarpart = NULL,
    temporal_hvarpart = NULL,
    unique_adjusted_r2 = tibble::tibble(),
    residual_moran = tibble::tibble()
  )

  return(res)
}
