#' @title Select dbMEM predictors conditional on covariates
#' @description
#' Residualise a response and candidate dbMEMs against supplied conditions,
#' test the complete spatial component, and apply adjusted-R-squared and
#' significance stopping to forward selection.
#' @param response Numeric vector, matrix, or data frame response.
#' @param mem_basis Numeric matrix or data frame of candidate dbMEMs.
#' @param conditions Optional numeric model matrix of conditioning variables.
#' @param permutations Number of permutations.
#' @param alpha Selection and global-test significance threshold.
#' @param min_residual_df Minimum residual degrees of freedom to preserve.
#' @param seed Integer random seed.
#' @return
#' A list with status, global test, candidate counts, selected names and table.
#' @examples
#' \dontrun{
#' select_dbmem_predictors(
#'   response = community,
#'   mem_basis = dbmem_values,
#'   conditions = human_climate
#' )
#' }
select_dbmem_predictors <- function(
  response,
  mem_basis,
  conditions = NULL,
  permutations = 999L,
  alpha = 0.05,
  min_residual_df = 10L,
  seed = 1234L
) {
  assertthat::assert_that(
    is.numeric(response) || is.matrix(response) || is.data.frame(response),
    is.matrix(mem_basis) || is.data.frame(mem_basis),
    is.null(conditions) ||
      is.matrix(conditions) || is.data.frame(conditions),
    is.numeric(permutations),
    length(permutations) == 1L,
    permutations >= 1L,
    is.numeric(alpha),
    length(alpha) == 1L,
    alpha > 0,
    alpha < 1,
    is.numeric(min_residual_df),
    length(min_residual_df) == 1L,
    min_residual_df >= 1L,
    msg = "dbMEM selection inputs do not satisfy the required contract."
  )

  mat_response <- as.matrix(response)
  mat_mem <- as.matrix(mem_basis)
  mat_conditions <-
    if (
      is.null(conditions)
    ) {
      matrix(1, nrow = nrow(mat_response), ncol = 1L)
    } else {
      as.matrix(conditions)
    }

  assertthat::assert_that(
    nrow(mat_response) == nrow(mat_mem),
    nrow(mat_response) == nrow(mat_conditions),
    ncol(mat_mem) > 0L,
    msg = "Response, dbMEMs, and conditions must have matching rows."
  )

  if (
    is.null(colnames(mat_mem))
  ) {
    colnames(mat_mem) <- sprintf("dbmem_%03d", seq_len(ncol(mat_mem)))
  }

  vec_complete <-
    stats::complete.cases(mat_response, mat_mem, mat_conditions)
  mat_response <- mat_response[vec_complete, , drop = FALSE]
  mat_mem <- mat_mem[vec_complete, , drop = FALSE]
  mat_conditions <- mat_conditions[vec_complete, , drop = FALSE]

  mat_response <-
    remove_constant_matrix_columns(data_matrix = mat_response)
  mat_mem <-
    remove_constant_matrix_columns(data_matrix = mat_mem)

  if (
    ncol(mat_response) == 0L || ncol(mat_mem) == 0L
  ) {
    return(
      create_empty_dbmem_selection(
        status_value = "rank_deficient",
        n_complete = nrow(mat_response),
        n_candidates = ncol(mat_mem)
      )
    )
  }

  rank_conditions <- qr(mat_conditions)[["rank"]]
  max_selected <-
    nrow(mat_response) - rank_conditions - min_residual_df
  if (
    max_selected < 1L
  ) {
    return(
      create_empty_dbmem_selection(
        status_value = "insufficient_residual_df",
        n_complete = nrow(mat_response),
        n_candidates = ncol(mat_mem)
      )
    )
  }
  max_selected <- min(max_selected, ncol(mat_mem))

  mat_response_residual <-
    qr.resid(qr(mat_conditions), mat_response)
  mat_mem_residual <-
    qr.resid(qr(mat_conditions), mat_mem)
  mat_mem_residual <-
    remove_constant_matrix_columns(data_matrix = mat_mem_residual)
  if (
    ncol(mat_mem_residual) == 0L
  ) {
    return(
      create_empty_dbmem_selection(
        status_value = "rank_deficient",
        n_complete = nrow(mat_response),
        n_candidates = ncol(mat_mem)
      )
    )
  }

  old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv)
  if (
    old_seed_exists
  ) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv)
  }
  on.exit(
    {
      if (
        old_seed_exists
      ) {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      } else if (
        exists(".Random.seed", envir = .GlobalEnv)
      ) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    },
    add = TRUE
  )
  set.seed(seed)

  model_global <-
    vegan::rda(
      X = mat_response_residual,
      Y = mat_mem_residual,
      scale = TRUE
    )
  global_test <-
    vegan::anova.cca(
      object = model_global,
      permutations = permutations
    )
  global_p_value <- as.numeric(global_test[["Pr(>F)"]][1])
  full_adjusted_r_squared <-
    vegan::RsquareAdj(model_global)[["adj.r.squared"]]

  if (
    !is.finite(global_p_value) ||
      global_p_value > alpha ||
      !is.finite(full_adjusted_r_squared) ||
      full_adjusted_r_squared <= 0
  ) {
    res_no_signal <-
      create_empty_dbmem_selection(
        status_value = "no_spatial_signal",
        n_complete = nrow(mat_response),
        n_candidates = ncol(mat_mem)
      )
    res_no_signal[["global_p_value"]] <- global_p_value
    res_no_signal[["full_adjusted_r_squared"]] <-
      full_adjusted_r_squared
    return(res_no_signal)
  }

  data_selection <-
    run_forward_dbmem_selection(
      response_residual = mat_response_residual,
      mem_residual = mat_mem_residual,
      max_selected = max_selected,
      adjusted_r_squared = full_adjusted_r_squared,
      permutations = permutations,
      alpha = alpha
    )

  if (
    nrow(data_selection) > 0L
  ) {
    vec_accepted <-
      cumprod(
        data_selection[["pvalue"]] <= alpha &
          data_selection[["AdjR2Cum"]] <=
          full_adjusted_r_squared + sqrt(.Machine[["double.eps"]])
      ) == 1
    data_selection <- data_selection[vec_accepted, , drop = FALSE]
  }

  selected_names <-
    if (
      nrow(data_selection) == 0L
    ) {
      character()
    } else {
      as.character(data_selection[["variables"]])
    }
  status <-
    if (
      length(selected_names) == 0L
    ) {
      "no_terms_selected"
    } else {
      "selected"
    }

  res_selection <-
    list(
      status = status,
      n_complete = nrow(mat_response),
      n_candidates = ncol(mat_mem_residual),
      global_p_value = global_p_value,
      full_adjusted_r_squared = full_adjusted_r_squared,
      selected_names = selected_names,
      selection_table = data_selection
    )

  return(res_selection)
}
