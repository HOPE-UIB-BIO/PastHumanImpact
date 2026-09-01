#' Prepare three-way matched human-event comparisons
#'
#' @param data_source All-available results containing cohort, proxy variant,
#'   status, metrics, and ranking fields.
#' @param key_cols Analytical-unit columns within each cohort.
#' @param metric_cols Numeric source fields to compare.
#' @param ranking_cols Character ranking fields to compare.
#' @param estimable_statuses Status values considered estimable.
#'
#' @return A three-way matched wide table with two contrasts against SPD.
#'
#' @export
prepare_human_event_matched_comparison <- function(
  data_source,
  key_cols,
  metric_cols,
  ranking_cols,
  estimable_statuses
) {
  required_columns <- c(
    "cohort",
    "proxy_variant",
    "status",
    key_cols,
    metric_cols,
    ranking_cols
  )
  assertthat::assert_that(
    is.data.frame(data_source),
    all(required_columns %in% names(data_source)),
    setequal(
      unique(data_source[["proxy_variant"]]),
      c("spd", "spd_events", "events")
    ),
    !anyDuplicated(data_source[c("cohort", key_cols, "proxy_variant")]),
    msg = "Human-event comparison inputs do not satisfy the contract."
  )

  comparison <-
    data_source |>
    dplyr::select(dplyr::all_of(required_columns)) |>
    tidyr::pivot_wider(
      names_from = "proxy_variant",
      values_from = dplyr::all_of(c("status", metric_cols, ranking_cols)),
      names_glue = "{.value}__{proxy_variant}"
    )

  for (metric in metric_cols) {
    comparison[[paste0(metric, "__spd_events_minus_spd")]] <-
      comparison[[paste0(metric, "__spd_events")]] -
      comparison[[paste0(metric, "__spd")]]
    comparison[[paste0(metric, "__events_minus_spd")]] <-
      comparison[[paste0(metric, "__events")]] -
      comparison[[paste0(metric, "__spd")]]
  }
  for (ranking in ranking_cols) {
    comparison[[paste0(ranking, "__spd_events_vs_spd_reversal")]] <-
      !is.na(comparison[[paste0(ranking, "__spd_events")]]) &
      !is.na(comparison[[paste0(ranking, "__spd")]]) &
      comparison[[paste0(ranking, "__spd_events")]] !=
        comparison[[paste0(ranking, "__spd")]]
    comparison[[paste0(ranking, "__events_vs_spd_reversal")]] <-
      !is.na(comparison[[paste0(ranking, "__events")]]) &
      !is.na(comparison[[paste0(ranking, "__spd")]]) &
      comparison[[paste0(ranking, "__events")]] !=
        comparison[[paste0(ranking, "__spd")]]
  }

  comparison |>
    dplyr::mutate(
      estimable__spd = .data[["status__spd"]] %in% estimable_statuses,
      estimable__spd_events =
        .data[["status__spd_events"]] %in% estimable_statuses,
      estimable__events = .data[["status__events"]] %in% estimable_statuses,
      three_way_estimable =
        .data[["estimable__spd"]] &
        .data[["estimable__spd_events"]] &
        .data[["estimable__events"]],
      estimability_change__spd_events_vs_spd =
        .data[["estimable__spd_events"]] != .data[["estimable__spd"]],
      estimability_change__events_vs_spd =
        .data[["estimable__events"]] != .data[["estimable__spd"]]
    )
}
