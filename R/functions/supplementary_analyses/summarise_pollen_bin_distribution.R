#' @title Summarise pollen grain-bin distributions
#' @description
#' Aggregates sample counts by grain bins and user-defined grouping columns,
#' returning counts and proportions within each grouping stratum.
#' @param data_source
#' A data frame containing `grain_bin` and grouping columns supplied in
#' `group_cols`.
#' @param group_cols
#' Character vector of grouping column names.
#' @return
#' A tibble with grouped sample counts (`n_samples`), grouped totals
#' (`n_samples_total`), and proportions (`prop_samples`).
#' @examples
#' \dontrun{
#' summarise_pollen_bin_distribution(
#'   data_source = data_samples,
#'   group_cols = c("stage", "region")
#' )
#' }
summarise_pollen_bin_distribution <- function(data_source,
                                              group_cols) {
  n_samples <- n_samples_total <- grain_bin <- NULL

  assertthat::assert_that(
    is.data.frame(data_source),
    msg = "`data_source` must be a data frame."
  )

  assertthat::assert_that(
    is.character(group_cols) && length(group_cols) > 0,
    msg = "`group_cols` must be a non-empty character vector."
  )

  assertthat::assert_that(
    "grain_bin" %in% names(data_source),
    msg = "`data_source` must contain `grain_bin`."
  )

  assertthat::assert_that(
    all(group_cols %in% names(data_source)),
    msg = "All `group_cols` must exist in `data_source`."
  )

  res_table <-
    data_source |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_cols, "grain_bin")))) |>
    dplyr::summarise(
      n_samples = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::mutate(
      n_samples_total = sum(n_samples),
      prop_samples = n_samples / n_samples_total
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(dplyr::across(dplyr::all_of(group_cols)), grain_bin)

  return(res_table)
}
