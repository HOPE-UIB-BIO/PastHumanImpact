#' @title Prepare paired sequence adjusted R-squared comparisons
#' @description Match canonical SPD-only and filtered joint-human results by
#' dataset and derive paired total and unique adjusted R-squared summaries.
#' @param canonical_balance Canonical SPD dataset-level balance table.
#' @param canonical_components Canonical long component-profile table.
#' @param filtered_balance Filtered joint-human dataset-level balance table.
#' @param filtered_unique_r2 Filtered joint-human unique adjusted R-squared.
#' @return A named list with paired values, long values, and summaries.
#' @examples
#' \dontrun{
#' prepare_sequence_r2_comparison(a, b, c, d)
#' }
prepare_sequence_r2_comparison <- function(
  canonical_balance,
  canonical_components,
  filtered_balance,
  filtered_unique_r2
) {
  inputs <- list(
    canonical_balance,
    canonical_components,
    filtered_balance,
    filtered_unique_r2
  )
  assertthat::assert_that(
    all(purrr::map_lgl(inputs, is.data.frame)),
    msg = "Sequence R-squared inputs must be data frames."
  )
  duplicate_balance <- dplyr::bind_rows(
    canonical_balance |>
      dplyr::count(.data[["dataset_id"]]) |>
      dplyr::filter(.data[["n"]] > 1L) |>
      dplyr::mutate(source = "canonical"),
    filtered_balance |>
      dplyr::count(.data[["dataset_id"]]) |>
      dplyr::filter(.data[["n"]] > 1L) |>
      dplyr::mutate(source = "filtered")
  )
  if (nrow(duplicate_balance) > 0L) {
    cli::cli_abort("Balance tables must contain one row per dataset_id.")
  }
  canonical_unique <- canonical_components |>
    dplyr::filter(
      .data[["measure"]] == "unique_adjusted_r2",
      .data[["component"]] %in% c("human", "climate", "time")
    ) |>
    dplyr::select(dplyr::all_of(c("dataset_id", "component", "value"))) |>
    tidyr::pivot_wider(
      names_from = "component",
      values_from = "value",
      names_prefix = "spd_"
    )
  filtered_unique <- filtered_unique_r2 |>
    dplyr::filter(
      .data[["fraction"]] %in% c(
        "pure_human", "pure_climate", "pure_time"
      )
    ) |>
    dplyr::mutate(
      fraction = stringr::str_remove(.data[["fraction"]], "^pure_")
    ) |>
    dplyr::transmute(
      dataset_id = .data[["dataset_id"]],
      fraction = .data[["fraction"]],
      value = .data[["adjusted_r_squared"]]
    ) |>
    tidyr::pivot_wider(
      names_from = "fraction",
      values_from = "value",
      names_prefix = "joint_"
    )
  sequence_values <- canonical_balance |>
    dplyr::transmute(
      dataset_id = .data[["dataset_id"]],
      spd_total = .data[["total_adjusted_r_squared"]],
      spd_signed_balance = .data[["signed_balance"]]
    ) |>
    dplyr::inner_join(
      filtered_balance |>
        dplyr::transmute(
          dataset_id = .data[["dataset_id"]],
          continental_region = .data[["region"]],
          region = .data[["climatezone"]],
          long = .data[["long"]],
          lat = .data[["lat"]],
          joint_total = .data[["total_adjusted_r_squared"]],
          joint_signed_balance = .data[["signed_balance"]]
        ),
      by = "dataset_id"
    ) |>
    dplyr::inner_join(canonical_unique, by = "dataset_id") |>
    dplyr::inner_join(filtered_unique, by = "dataset_id") |>
    dplyr::mutate(
      delta_total = .data[["joint_total"]] - .data[["spd_total"]],
      delta_human = .data[["joint_human"]] - .data[["spd_human"]],
      delta_climate = .data[["joint_climate"]] - .data[["spd_climate"]],
      delta_time = .data[["joint_time"]] - .data[["spd_time"]],
      delta_signed_balance =
        .data[["joint_signed_balance"]] - .data[["spd_signed_balance"]]
    ) |>
    dplyr::arrange(.data[["continental_region"]], .data[["dataset_id"]])
  metric_labels <- c(
    total = "Total adjusted R²",
    human = "Pure human adjusted R²",
    climate = "Pure climate adjusted R²",
    time = "Pure time adjusted R²"
  )
  comparison_long <- purrr::map_dfr(
    names(metric_labels),
    .f = ~ sequence_values |>
      dplyr::transmute(
        dataset_id = .data[["dataset_id"]],
        continental_region = .data[["continental_region"]],
        region = .data[["region"]],
        metric = .env[["metric_labels"]][[.x]],
        spd_only = .data[[paste0("spd_", .x)]],
        filtered_joint = .data[[paste0("joint_", .x)]],
        difference = .data[[paste0("delta_", .x)]]
      )
  ) |>
    dplyr::mutate(
      metric = factor(.data[["metric"]], levels = unname(metric_labels))
    )
  r2_summary <- comparison_long |>
    dplyr::summarise(
      n_sequences = sum(stats::complete.cases(
        .data[["spd_only"]], .data[["filtered_joint"]]
      )),
      correlation = if (
        dplyr::n_distinct(
          .data[["spd_only"]][is.finite(.data[["spd_only"]])]
        ) > 1L &&
          dplyr::n_distinct(
            .data[["filtered_joint"]][is.finite(.data[["filtered_joint"]])]
          ) > 1L
      ) {
        stats::cor(
          .data[["spd_only"]],
          .data[["filtered_joint"]],
          use = "complete.obs"
        )
      } else {
        NA_real_
      },
      median_spd_only = stats::median(.data[["spd_only"]], na.rm = TRUE),
      median_filtered_joint = stats::median(
        .data[["filtered_joint"]], na.rm = TRUE
      ),
      median_difference = stats::median(.data[["difference"]], na.rm = TRUE),
      q025_difference = stats::quantile(
        .data[["difference"]], probs = 0.025, na.rm = TRUE
      ),
      q975_difference = stats::quantile(
        .data[["difference"]], probs = 0.975, na.rm = TRUE
      ),
      .by = "metric"
    )
  res <- list(
    sequence_values = sequence_values,
    comparison_long = comparison_long,
    r2_summary = r2_summary
  )

  return(res)
}
