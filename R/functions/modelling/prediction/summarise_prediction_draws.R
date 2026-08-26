#' @title Summarise posterior predictions across datasets
#' @description
#' Average posterior expected responses equally across datasets within each
#' prediction cell, then calculate posterior summaries.
#' @param mat_draws Numeric matrix with posterior draws in rows and prediction
#' observations in columns.
#' @param data_new Prediction data corresponding to columns of `mat_draws`.
#' @param group_var Optional character scalar dataset grouping column. When
#' `NULL`, each prediction row is summarised without dataset averaging.
#' @param probs Numeric length-two vector of interval probabilities.
#' @return Data frame with one row per non-dataset prediction cell, posterior
#' summaries, and the number of datasets marginalised.
#' @details
#' When `group_var` is supplied, each dataset receives equal weight at every
#' prediction cell. Summaries are calculated after averaging within draws,
#' preserving posterior dependence among dataset-specific trajectories.
#' @examples
#' data_summary <- summarise_prediction_draws(
#'   mat_draws = matrix(c(1, 3, 2, 4), nrow = 2),
#'   data_new = data.frame(age = c(0, 0), dataset_id = c("a", "b")),
#'   group_var = "dataset_id"
#' )
summarise_prediction_draws <- function(
  mat_draws,
  data_new,
  group_var = NULL,
  probs = c(0.025, 0.975)
) {
  assertthat::assert_that(
    is.matrix(mat_draws),
    is.numeric(mat_draws),
    nrow(mat_draws) > 0L,
    msg = "`mat_draws` must be a non-empty numeric matrix."
  )
  assertthat::assert_that(
    is.data.frame(data_new),
    nrow(data_new) == ncol(mat_draws),
    msg = "Rows of `data_new` must match columns of `mat_draws`."
  )
  assertthat::assert_that(
    is.null(group_var) ||
      (
        is.character(group_var) &&
          length(group_var) == 1L &&
          group_var %in% names(data_new)
      ),
    msg = "`group_var` must be NULL or identify one column in `data_new`."
  )
  assertthat::assert_that(
    is.numeric(probs),
    length(probs) == 2L,
    all(is.finite(probs)),
    probs[1] > 0,
    probs[2] < 1,
    probs[1] < probs[2],
    msg = "`probs` must contain two increasing probabilities within (0, 1)."
  )
  assertthat::assert_that(
    all(is.finite(mat_draws)),
    is.null(group_var) || all(!is.na(data_new[[group_var]])),
    msg = "Prediction draws and group identifiers must be non-missing."
  )

  if (
    is.null(group_var)
  ) {
    prediction_columns <-
      names(data_new)
    data_index <-
      data_new %>%
      dplyr::mutate(.prediction_group = dplyr::row_number())
  } else {
    prediction_columns <-
      setdiff(names(data_new), group_var)
    data_index <-
      data_new %>%
      dplyr::group_by(
        dplyr::across(dplyr::all_of(prediction_columns))
      ) %>%
      dplyr::mutate(.prediction_group = dplyr::cur_group_id()) %>%
      dplyr::ungroup()
  }

  assertthat::assert_that(
    length(prediction_columns) > 0L,
    msg = "`data_new` must contain a non-dataset prediction column."
  )

  mat_weights <-
    stats::model.matrix(
      ~ 0 + factor(.prediction_group),
      data = data_index
    )
  mat_weights <-
    sweep(
      mat_weights,
      MARGIN = 2L,
      STATS = colSums(mat_weights),
      FUN = "/"
    )
  mat_marginal_draws <-
    mat_draws %*% mat_weights

  if (
    is.null(group_var)
  ) {
    data_groups <-
      data_index %>%
      dplyr::mutate(n_datasets_marginalised = 1L)
  } else {
    data_groups <-
      data_index %>%
      dplyr::group_by(.prediction_group) %>%
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(prediction_columns),
          dplyr::first
        ),
        n_datasets_marginalised =
          dplyr::n_distinct(.data[[group_var]]),
        .groups = "drop"
      )
  }

  data_summary <-
    seq_len(ncol(mat_marginal_draws)) %>%
    purrr::map(
      .f = ~ tibble::tibble(
        .prediction_group = .x,
        estimate = mean(mat_marginal_draws[, .x]),
        estimate_error = stats::sd(mat_marginal_draws[, .x]),
        conf_low = stats::quantile(
          mat_marginal_draws[, .x],
          probs = probs[1],
          names = FALSE
        ),
        conf_high = stats::quantile(
          mat_marginal_draws[, .x],
          probs = probs[2],
          names = FALSE
        )
      )
    ) |>
    dplyr::bind_rows()

  res_data <-
    data_groups %>%
    dplyr::left_join(
      data_summary,
      by = dplyr::join_by(.prediction_group)
    ) %>%
    dplyr::select(-.prediction_group)

  return(res_data)
}
