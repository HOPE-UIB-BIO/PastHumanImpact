#' @title Calculate shared HVarPart display limits
#' @description
#' Calculate global central display limits and explicit lower/upper tail counts
#' without removing values from the supplied data.
#' @param data_source Data frame containing the signed allocation column.
#' @param value_col Name of the signed allocation column.
#' @param probabilities Two probabilities defining the central interval.
#' @param rounding Positive increment used to round limits outward.
#' @return A list containing `limits` and a one-row `tail_counts` tibble.
get_hvarpart_display_limits <- function(
  data_source,
  value_col = "signed_allocation",
  probabilities = c(0.01, 0.99),
  rounding = 0.05
) {
  assertthat::assert_that(
    is.data.frame(data_source),
    value_col %in% names(data_source),
    is.numeric(data_source[[value_col]]),
    length(probabilities) == 2L,
    all(is.finite(probabilities)),
    probabilities[[1L]] < probabilities[[2L]],
    rounding > 0,
    msg = "Invalid input for HVarPart display limits."
  )

  values <- data_source[[value_col]]
  values <- values[is.finite(values)]

  assertthat::assert_that(
    length(values) > 0L,
    msg = "No finite HVarPart allocations are available."
  )

  raw_limits <- stats::quantile(
    values,
    probs = probabilities,
    names = FALSE
  )
  limits <- c(
    floor(raw_limits[[1L]] / rounding) * rounding,
    ceiling(raw_limits[[2L]] / rounding) * rounding
  )

  tail_counts <-
    tibble::tibble(
      display_min = limits[[1L]],
      display_max = limits[[2L]],
      n_below_display = sum(values < limits[[1L]]),
      n_above_display = sum(values > limits[[2L]]),
      n_displayed = sum(
        values >= limits[[1L]] & values <= limits[[2L]]
      ),
      n_total = length(values)
    )

  return(
    list(
      limits = limits,
      tail_counts = tail_counts
    )
  )
}
