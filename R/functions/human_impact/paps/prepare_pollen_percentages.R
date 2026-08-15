#' @title Prepare pollen percentages
#' @description
#' Convert pollen counts to row-wise percentages and remove taxa absent from
#' every sample.
#' @param data_source_counts Data frame containing `sample_id` and numeric taxa.
#' @return A data frame containing `sample_id` and percentage-valued taxa.
#' @examples
#' prepare_pollen_percentages(
#'   data.frame(sample_id = c("a", "b"), taxon = c(2, 4))
#' )
prepare_pollen_percentages <- function(data_source_counts) {
  assertthat::assert_that(
    is.data.frame(data_source_counts),
    "sample_id" %in% names(data_source_counts),
    msg = "`data_source_counts` must be a data frame with `sample_id`."
  )

  vec_taxa <-
    setdiff(
      names(data_source_counts),
      "sample_id"
    )

  assertthat::assert_that(
    length(vec_taxa) > 0L,
    all(
      purrr::map_lgl(
        data_source_counts[vec_taxa],
        is.numeric
      )
    ),
    msg = "Pollen taxon columns must be numeric."
  )

  mat_counts <-
    data.matrix(data_source_counts[vec_taxa])

  vec_row_totals <-
    rowSums(mat_counts)

  assertthat::assert_that(
    all(is.finite(mat_counts)),
    all(mat_counts >= 0),
    all(vec_row_totals > 0),
    msg = "Pollen counts must be finite, non-negative, and have positive rows."
  )

  vec_retained_taxa <-
    colSums(mat_counts) > 0

  mat_percentages <-
    sweep(
      mat_counts[, vec_retained_taxa, drop = FALSE],
      MARGIN = 1L,
      STATS = vec_row_totals,
      FUN = "/"
    ) * 100

  data_percentages <-
    data.frame(
      sample_id = data_source_counts[["sample_id"]],
      mat_percentages,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

  return(data_percentages)
}
