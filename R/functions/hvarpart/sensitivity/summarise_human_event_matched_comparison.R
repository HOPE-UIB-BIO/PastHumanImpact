#' Summarise complete human-event contrast distributions
#'
#' @param data_comparison Three-way matched comparison table.
#' @param metric_cols Source metric names used to construct contrasts.
#' @param group_cols Optional geographic grouping columns.
#' @param summary_level Label describing the aggregation level.
#'
#' @return Long table of distribution summaries for both contrasts.
#'
#' @export
summarise_human_event_matched_comparison <- function(
  data_comparison,
  metric_cols,
  group_cols = character(),
  summary_level = "overall"
) {
  contrast_columns <-
    tidyr::crossing(
      metric = metric_cols,
      contrast = c("spd_events_minus_spd", "events_minus_spd")
    ) |>
    dplyr::mutate(column = paste(.data[["metric"]], .data[["contrast"]],
      sep = "__"))
  assertthat::assert_that(
    is.data.frame(data_comparison),
    all(c("cohort", group_cols, contrast_columns[["column"]]) %in%
      names(data_comparison)),
    msg = "Human-event summary inputs do not satisfy the contract."
  )

  if ("three_way_estimable" %in% names(data_comparison)) {
    data_comparison <-
      data_comparison |>
      dplyr::filter(.data[["three_way_estimable"]])
  }

  data_long <-
    data_comparison |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(contrast_columns[["column"]]),
      names_to = c("metric", "contrast"),
      names_pattern = "^(.*)__(spd_events_minus_spd|events_minus_spd)$",
      values_to = "difference"
    )

  data_long |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c(group_cols, "metric", "contrast")))
      ,
      .data[["cohort"]]
    ) |>
    dplyr::summarise(
      n_units = dplyr::n(),
      n_finite = sum(is.finite(.data[["difference"]])),
      minimum = min(.data[["difference"]], na.rm = TRUE),
      lower_quartile = stats::quantile(
        .data[["difference"]],
        probs = 0.25,
        na.rm = TRUE,
        names = FALSE
      ),
      median = stats::median(.data[["difference"]], na.rm = TRUE),
      mean = mean(.data[["difference"]], na.rm = TRUE),
      upper_quartile = stats::quantile(
        .data[["difference"]],
        probs = 0.75,
        na.rm = TRUE,
        names = FALSE
      ),
      maximum = max(.data[["difference"]], na.rm = TRUE),
      n_negative = sum(.data[["difference"]] < 0, na.rm = TRUE),
      n_zero = sum(.data[["difference"]] == 0, na.rm = TRUE),
      n_positive = sum(.data[["difference"]] > 0, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(summary_level = summary_level, .before = 1L) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(c(
          "minimum",
          "lower_quartile",
          "median",
          "mean",
          "upper_quartile",
          "maximum"
        )),
        ~ dplyr::if_else(.data[["n_finite"]] == 0L, NA_real_, .x)
      )
    )
}
