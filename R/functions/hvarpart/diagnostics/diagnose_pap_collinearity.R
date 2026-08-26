#' @title Summarise PAP collinearity by groups
#' @description
#' Computes pairwise PAP correlations with `collinear::cor_df()` and derives
#' a reduced predictor set with `collinear::collinear_select()` for each
#' grouping unit.
#' @param data_source Data frame containing grouping variables and PAP columns.
#' @param pap_vars Character vector of PAP variable names.
#' @param group_var Character vector of grouping column names.
#' @param preference_order Character vector ranking PAPs to retain first.
#' @param max_cor Numeric in (0,1). Pairwise correlation threshold.
#' @param max_vif Numeric. VIF threshold used by `collinear_select()`.
#' @param min_rows Integer. Minimum rows required per group.
#' @param quiet Logical. Passed to `collinear` functions.
#' @return
#' List with `group_diagnostics`, `correlation_table`,
#' `high_collinearity_pairs`, and `selection_table`.
#' @examples
#' \dontrun{
#' data_input <-
#'   tibble::tibble(
#'     region = rep(c("A", "B"), each = 20),
#'     n0 = stats::rnorm(40),
#'     n1 = stats::rnorm(40),
#'     n2 = stats::rnorm(40),
#'     roc = stats::rnorm(40)
#'   )
#'
#' res_collinearity <-
#'   diagnose_pap_collinearity(
#'     data_source = data_input,
#'     pap_vars = c("n0", "n1", "n2", "roc"),
#'     group_var = "region",
#'     preference_order = c("n0", "n1", "n2", "roc"),
#'     max_cor = 0.8,
#'     max_vif = 5,
#'     min_rows = 10,
#'     quiet = TRUE
#'   )
#' }

diagnose_pap_collinearity <- function(data_source,
                                 pap_vars = c(
                                   "n0",
                                   "n1",
                                   "n2",
                                   "n1_minus_n2",
                                   "n2_divided_by_n1",
                                   "n1_divided_by_n0",
                                   "roc",
                                   "dcca_axis_1",
                                   "density_diversity",
                                   "density_turnover"
                                 ),
                                 group_var = c("region"),
                                 preference_order = pap_vars,
                                 max_cor = 0.8,
                                 max_vif = 5,
                                 min_rows = 30,
                                 quiet = TRUE) {
  assertthat::assert_that(
    is.data.frame(data_source),
    msg = "`data_source` must be a data frame."
  )
  assertthat::assert_that(
    is.character(pap_vars),
    length(pap_vars) > 1,
    msg = "`pap_vars` must be a character vector with >=2 variables."
  )
  assertthat::assert_that(
    is.character(group_var),
    length(group_var) > 0,
    msg = "`group_var` must be a non-empty character vector."
  )
  assertthat::assert_that(
    all(c(group_var, pap_vars) %in% names(data_source)),
    msg = "Columns in `group_var` and `pap_vars` must exist in data_source."
  )
  assertthat::assert_that(
    is.character(preference_order),
    all(preference_order %in% pap_vars),
    msg = "`preference_order` must be a subset of `pap_vars`."
  )
  assertthat::assert_that(
    is.numeric(max_cor),
    length(max_cor) == 1,
    max_cor > 0,
    max_cor < 1,
    msg = "`max_cor` must be a single number between 0 and 1."
  )
  assertthat::assert_that(
    is.numeric(max_vif),
    length(max_vif) == 1,
    max_vif > 0,
    msg = "`max_vif` must be a positive number."
  )
  assertthat::assert_that(
    is.numeric(min_rows),
    length(min_rows) == 1,
    min_rows >= 10,
    msg = "`min_rows` must be >= 10."
  )
  assertthat::assert_that(
    is.logical(quiet),
    length(quiet) == 1,
    !is.na(quiet),
    msg = "`quiet` must be TRUE or FALSE."
  )

  if (!requireNamespace("collinear", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg collinear} is required. Install with install.packages('collinear')."
    )
  }

  data_nested <-
    data_source |>
    dplyr::select(
      dplyr::all_of(c(group_var, pap_vars))
    ) |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(group_var))
    ) |>
    tidyr::nest(data_group = dplyr::all_of(pap_vars)) |>
    dplyr::ungroup()

  data_nested[["n_rows"]] <-
    purrr::map_int(data_nested[["data_group"]], nrow)
    
  data_nested[["is_eligible"]] <-
    data_nested[["n_rows"]] >= min_rows

  data_nested[["correlation_table"]] <-
    purrr::map2(
      .x = data_nested[["data_group"]],
      .y = data_nested[["is_eligible"]],
      .f = ~ {
        if (!isTRUE(.y)) {
          res <-
            tibble::tibble()

          return(res)
        }

        collinear::cor_df(
          df = .x,
          predictors = pap_vars,
          quiet = quiet
        )
      }
    )

  data_nested[["selected_vars"]] <-
    purrr::map2(
      .x = data_nested[["data_group"]],
      .y = data_nested[["is_eligible"]],
      .f = ~ {
        if (!isTRUE(.y)) {
          res <-
            character()

          return(res)
        }

        collinear::collinear_select(
          df = .x,
          predictors = pap_vars,
          preference_order = preference_order,
          max_cor = max_cor,
          max_vif = max_vif,
          quiet = quiet
        ) |>
          unname()
      }
    )

  correlation_table <-
    purrr::map(
      .x = seq_len(nrow(data_nested)),
      .f = ~ {
        data_group_info <-
          data_nested[.x, c(group_var, "n_rows", "is_eligible"), drop = FALSE]

        data_correlation <- data_nested[["correlation_table"]][[.x]]

        if (nrow(data_correlation) == 0) {
          data_correlation <-
            tibble::tibble(
              x = NA_character_,
              y = NA_character_,
              correlation = NA_real_,
              metric = NA_character_
            )
        }

        dplyr::bind_cols(
          data_group_info,
          data_correlation
        )
      }
    ) |>
    dplyr::bind_rows()

  correlation_table[["abs_correlation"]] <-
    abs(correlation_table[["correlation"]])

  selection_table <-
    purrr::map(
      .x = seq_len(nrow(data_nested)),
      .f = ~ {
        data_group_info <-
          data_nested[.x, c(group_var, "n_rows", "is_eligible"), drop = FALSE]

        selected_vars <- data_nested[["selected_vars"]][[.x]]

        if (length(selected_vars) == 0) {
          selected_vars <- NA_character_
        }

        tibble::tibble(
          data_group_info,
          predictor = selected_vars
        )
      }
    ) |>
    dplyr::bind_rows()

  high_collinearity_pairs <-
    correlation_table[
      correlation_table[["is_eligible"]] &
        !is.na(correlation_table[["abs_correlation"]]) &
        correlation_table[["abs_correlation"]] >= max_cor, ,
      drop = FALSE
    ]

  group_diagnostics <-
    data_nested[, c(group_var, "n_rows", "is_eligible"), drop = FALSE]

  res_output <-
    list(
      group_diagnostics = group_diagnostics,
      correlation_table = correlation_table,
      high_collinearity_pairs = high_collinearity_pairs,
      selection_table = selection_table
    )

  return(res_output)
}
