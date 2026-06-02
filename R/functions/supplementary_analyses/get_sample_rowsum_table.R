#' @title Compute sample-level pollen grain sums
#' @description
#' Computes row-wise pollen grain sums for a count table containing a
#' `sample_id` column and taxon count columns.
#' @param data_counts
#' A data frame with one row per sample, a `sample_id` column, and numeric
#' count columns for taxa.
#' @return
#' A tibble with columns `sample_id` and `rowsum`.
#' @examples
#' \dontrun{
#' data_counts <-
#'   tibble::tibble(
#'     sample_id = c("s1", "s2"),
#'     taxon_a = c(1, 2),
#'     taxon_b = c(3, 4)
#'   )
#'
#' get_sample_rowsum_table(data_counts = data_counts)
#' }
get_sample_rowsum_table <- function(data_counts) {
  assertthat::assert_that(
    is.data.frame(data_counts),
    msg = "`data_counts` must be a data frame."
  )

  assertthat::assert_that(
    "sample_id" %in% names(data_counts),
    msg = "`data_counts` must include `sample_id`."
  )

  vec_rowsum <-
    data_counts |>
    dplyr::select(-dplyr::all_of("sample_id")) |>
    rowSums(na.rm = TRUE) |>
    as.numeric()

  res_table <-
    tibble::tibble(
      sample_id = data_counts[["sample_id"]],
      rowsum = vec_rowsum
    )

  return(res_table)
}
