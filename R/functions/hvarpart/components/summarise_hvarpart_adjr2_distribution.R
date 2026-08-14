#' @title Summarise adjusted R-squared distributions
#' @description
#' Calculate model counts and distribution summaries for adjusted R-squared,
#' optionally within groups.
#' @param data_values Model-level data containing `adjusted_r_squared`.
#' @param group_vars Optional grouping columns.
#' @return A tibble with counts, mean, standard deviation, minimum, quartiles,
#' median, and maximum.
summarise_hvarpart_adjr2_distribution <- function(
  data_values,
  group_vars = character()
) {
  assertthat::assert_that(
    is.data.frame(data_values),
    "adjusted_r_squared" %in% names(data_values),
    all(group_vars %in% names(data_values)),
    msg = "`data_values` does not satisfy the distribution contract."
  )

  data_finite <-
    data_values |>
    dplyr::filter(is.finite(.data[["adjusted_r_squared"]]))

  assertthat::assert_that(
    nrow(data_finite) > 0L,
    msg = "No finite adjusted R-squared values are available."
  )

  summary <-
    data_finite |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(
      n_models = dplyr::n(),
      mean_adjusted_r_squared = mean(.data[["adjusted_r_squared"]]),
      sd_adjusted_r_squared = stats::sd(.data[["adjusted_r_squared"]]),
      min_adjusted_r_squared = min(.data[["adjusted_r_squared"]]),
      q1_adjusted_r_squared = stats::quantile(
        .data[["adjusted_r_squared"]],
        probs = 0.25,
        names = FALSE
      ),
      median_adjusted_r_squared = stats::median(
        .data[["adjusted_r_squared"]]
      ),
      q3_adjusted_r_squared = stats::quantile(
        .data[["adjusted_r_squared"]],
        probs = 0.75,
        names = FALSE
      ),
      max_adjusted_r_squared = max(.data[["adjusted_r_squared"]]),
      .groups = "drop"
    )

  return(summary)
}
