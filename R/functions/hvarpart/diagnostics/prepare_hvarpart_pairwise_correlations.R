#' @title Prepare pairwise correlations for an HVarPart design
#' @description Compute pairwise correlations and conceptual-group labels for
#' a supplied set of predictor and protected-control variables.
#' @param data_source Data frame containing the variables.
#' @param variables Variable names to compare.
#' @param predictor_vars Named human and climate predictor groups.
#' @param control_vars Protected control-variable names.
#' @param stage Diagnostic-stage label.
#' @param correlation_threshold Absolute-correlation warning threshold.
#' @return A tibble with one row per variable pair.
#' @examples
#' prepare_hvarpart_pairwise_correlations(
#'   data.frame(h = 1:5, c = 5:1), c("h", "c"),
#'   list(human = "h", climate = "c"), character(), "selected"
#' )
prepare_hvarpart_pairwise_correlations <- function(
  data_source,
  variables,
  predictor_vars,
  control_vars,
  stage,
  correlation_threshold = 0.8
) {
  assertthat::assert_that(
    is.data.frame(data_source),
    is.character(variables),
    all(variables %in% names(data_source)),
    is.list(predictor_vars),
    all(c("human", "climate") %in% names(predictor_vars)),
    is.character(control_vars),
    assertthat::is.string(stage),
    is.numeric(correlation_threshold),
    length(correlation_threshold) == 1L,
    msg = "Pairwise HVarPart correlation inputs do not satisfy the contract."
  )
  if (length(variables) < 2L) return(tibble::tibble())

  combinations <- utils::combn(variables, 2L, simplify = FALSE)
  res <- purrr::map_dfr(
    combinations,
    .f = ~ {
      pair <- .x
      values <- data_source[pair]
      complete <- stats::complete.cases(values) &
        is.finite(values[[1]]) & is.finite(values[[2]])
      correlation <- if (sum(complete) < 3L) {
        NA_real_
      } else {
        stats::cor(values[[1]][complete], values[[2]][complete])
      }
      groups <- dplyr::case_when(
        pair %in% predictor_vars$human ~ "human",
        pair %in% predictor_vars$climate ~ "climate",
        pair %in% control_vars ~ "control",
        .default = "candidate"
      )
      tibble::tibble(
        stage = stage,
        variable_1 = pair[[1]],
        variable_2 = pair[[2]],
        group_1 = groups[[1]],
        group_2 = groups[[2]],
        n_complete = sum(complete),
        correlation = correlation,
        high_correlation = is.finite(correlation) &
          abs(correlation) > correlation_threshold
      )
    }
  )

  return(res)
}
