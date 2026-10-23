#' @title Diagnose an HVarPart design matrix
#' @description Compute VIFs, condition indices, rank, and residual degrees of
#' freedom for finite, non-constant design columns.
#' @param data_source Data frame containing candidate variables.
#' @param variables Candidate design-column names.
#' @param stage Diagnostic-stage label.
#' @param vif_threshold VIF warning threshold.
#' @param condition_threshold Condition-index warning threshold.
#' @return A named list containing VIF, condition-index, and design tables.
#' @examples
#' diagnose_hvarpart_design_matrix(
#'   data.frame(a = 1:10, b = stats::rnorm(10)), c("a", "b"), "selected"
#' )
diagnose_hvarpart_design_matrix <- function(
  data_source,
  variables,
  stage,
  vif_threshold = 5,
  condition_threshold = 30
) {
  assertthat::assert_that(
    is.data.frame(data_source),
    is.character(variables),
    all(variables %in% names(data_source)),
    assertthat::is.string(stage),
    is.numeric(vif_threshold), length(vif_threshold) == 1L,
    is.numeric(condition_threshold), length(condition_threshold) == 1L,
    msg = "HVarPart design-diagnostic inputs do not satisfy the contract."
  )
  variables <- variables[purrr::map_lgl(
    variables,
    .f = ~ {
      values <- data_source[[.x]]
      length(values) > 1L && all(is.finite(values)) &&
        dplyr::n_distinct(values) > 1L
    }
  )]
  if (length(variables) == 0L) {
    res_empty <- list(
      vif = tibble::tibble(),
      condition = tibble::tibble(),
      design = tibble::tibble()
    )
    return(res_empty)
  }
  complete <- stats::complete.cases(data_source[variables])
  matrix_values <- as.matrix(data_source[complete, variables, drop = FALSE])
  if (nrow(matrix_values) < 2L) {
    res_short <- list(
      vif = tibble::tibble(),
      condition = tibble::tibble(),
      design = tibble::tibble(
        stage = stage,
        n_complete = nrow(matrix_values),
        n_columns = length(variables),
        design_rank = NA_integer_,
        residual_df = NA_integer_,
        full_rank = FALSE
      )
    )
    return(res_short)
  }

  design_rank <- qr(cbind(1, matrix_values))$rank
  residual_df <- nrow(matrix_values) - design_rank
  vif <- purrr::map_dfr(
    seq_along(variables),
    .f = ~ {
      index <- .x
      value <- if (length(variables) == 1L) {
        1
      } else if (nrow(matrix_values) < 3L) {
        NA_real_
      } else {
        response <- matrix_values[, index]
        other <- matrix_values[, -index, drop = FALSE]
        r_squared <- summary(stats::lm(response ~ other))$r.squared
        if (isTRUE(all.equal(r_squared, 1))) Inf else 1 / (1 - r_squared)
      }
      tibble::tibble(
        stage = stage,
        predictor = variables[[index]],
        vif = value,
        high_vif = !is.finite(value) | value > vif_threshold
      )
    }
  )
  scaled <- scale(matrix_values)
  singular_values <- if (all(is.finite(scaled))) {
    svd(scaled, nu = 0, nv = 0)$d
  } else {
    numeric()
  }
  condition <- if (length(singular_values) == 0L) {
    numeric()
  } else {
    max(singular_values) / singular_values
  }
  res <- list(
    vif = vif,
    condition = tibble::tibble(
      stage = stage,
      dimension = seq_along(condition),
      condition_index = condition,
      high_condition_index = !is.finite(condition) |
        condition > condition_threshold
    ),
    design = tibble::tibble(
      stage = stage,
      n_complete = nrow(matrix_values),
      n_columns = length(variables),
      design_rank = design_rank,
      residual_df = residual_df,
      full_rank = design_rank == length(variables) + 1L
    )
  )

  return(res)
}
