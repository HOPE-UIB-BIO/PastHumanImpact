#' Reconstruct human-event summaries from matched source rows
#'
#' @param data_comparison Three-way matched comparison table.
#' @param data_summary Published distribution summary table.
#' @param tolerance Numeric equality tolerance.
#'
#' @return Row-level reconciliation diagnostics.
#'
#' @export
diagnose_human_event_summary_reconciliation <- function(
  data_comparison,
  data_summary,
  tolerance = 1e-12
) {
  value_columns <- c(
    "minimum",
    "lower_quartile",
    "median",
    "mean",
    "upper_quartile",
    "maximum"
  )
  count_columns <- c("n_units", "n_finite", "n_negative", "n_zero", "n_positive")
  required_summary <- c(
    "summary_level",
    "cohort",
    "metric",
    "contrast",
    value_columns,
    count_columns
  )
  assertthat::assert_that(
    is.data.frame(data_comparison),
    is.data.frame(data_summary),
    all(required_summary %in% names(data_summary)),
    msg = "Human-event reconciliation inputs do not satisfy the contract."
  )

  if ("three_way_estimable" %in% names(data_comparison)) {
    data_comparison <-
      data_comparison |>
      dplyr::filter(.data[["three_way_estimable"]])
  }

  grouping_candidates <- intersect(
    c("region", "climatezone", "age"),
    names(data_summary)
  )
  result <-
    seq_len(nrow(data_summary)) |>
    purrr::map_dfr(
      function(index) {
        summary_row <- data_summary[index, , drop = FALSE]
        source <-
          data_comparison |>
          dplyr::filter(
            .data[["cohort"]] == summary_row[["cohort"]][[1]]
          )
        for (group_column in grouping_candidates) {
          group_value <- summary_row[[group_column]][[1]]
          if (!is.na(group_value)) {
            source <- source |>
              dplyr::filter(.data[[group_column]] == group_value)
          }
        }
        difference_column <- paste(
          summary_row[["metric"]][[1]],
          summary_row[["contrast"]][[1]],
          sep = "__"
        )
        values <- source[[difference_column]]
        finite_values <- values[is.finite(values)]
        reconstructed_values <- c(
          minimum = if (length(finite_values)) min(finite_values) else NA_real_,
          lower_quartile = if (length(finite_values)) {
            stats::quantile(finite_values, 0.25, names = FALSE)
          } else {
            NA_real_
          },
          median = if (length(finite_values)) {
            stats::median(finite_values)
          } else {
            NA_real_
          },
          mean = if (length(finite_values)) mean(finite_values) else NA_real_,
          upper_quartile = if (length(finite_values)) {
            stats::quantile(finite_values, 0.75, names = FALSE)
          } else {
            NA_real_
          },
          maximum = if (length(finite_values)) max(finite_values) else NA_real_
        )
        reconstructed_counts <- c(
          n_units = length(values),
          n_finite = length(finite_values),
          n_negative = sum(values < 0, na.rm = TRUE),
          n_zero = sum(values == 0, na.rm = TRUE),
          n_positive = sum(values > 0, na.rm = TRUE)
        )
        published_values <- unlist(summary_row[value_columns], use.names = TRUE)
        published_counts <- unlist(summary_row[count_columns], use.names = TRUE)
        value_difference <- abs(published_values - reconstructed_values)
        value_match <-
          (is.na(published_values) & is.na(reconstructed_values)) |
          value_difference <= tolerance
        count_match <- published_counts == reconstructed_counts

        tibble::tibble(
          summary_index = index,
          summary_level = summary_row[["summary_level"]],
          cohort = summary_row[["cohort"]],
          metric = summary_row[["metric"]],
          contrast = summary_row[["contrast"]],
          n_source_rows = length(values),
          maximum_absolute_difference = max(value_difference, na.rm = TRUE),
          reconciled = all(value_match) && all(count_match)
        )
      }
    ) |>
    dplyr::mutate(
      maximum_absolute_difference = dplyr::if_else(
        is.infinite(.data[["maximum_absolute_difference"]]),
        0,
        .data[["maximum_absolute_difference"]]
      )
    )

  if (any(!result[["reconciled"]])) {
    cli::cli_abort("A human-event summary could not be reconstructed.")
  }

  result
}
