#' @title Compute multivariate regression trees
#' @description
#' Fit one legacy multivariate regression tree per pollen dataset and assemble
#' stable plain-data summaries.
#' @param data_pollen Data frame containing `dataset_id` and list-columns
#'   `percentages_harmonised` and `levels`.
#' @param n_rand Positive number of cross-validation repetitions.
#' @param transformation_coef One of `chisq`, `hellinger`, or `none`.
#' @param fit_dataset Function used to fit one pollen dataset.
#' @return A data frame containing dataset partitions, change points, and group
#'   counts.
#' @details
#' The underlying `mvpart` package requires the isolated old-R runtime
#' documented in `R/analyses/01_data_preparation/05_paps/README.md`. Do not
#' source the project-wide configuration from that runtime.
#' @examples
#' \dontrun{
#' compute_mrt(data_pollen)
#' }
compute_mrt <- function(
    data_pollen,
    n_rand = 999,
    transformation_coef = "chisq",
    fit_dataset = fit_mvpart_mrt
) {
  assertthat::assert_that(
    is.data.frame(data_pollen),
    all(
      c(
        "dataset_id",
        "percentages_harmonised",
        "levels"
      ) %in% names(data_pollen)
    ),
    msg = paste(
      "`data_pollen` must contain `dataset_id`,",
      "`percentages_harmonised`, and `levels`."
    )
  )

  assertthat::assert_that(
    all(
      purrr::map_lgl(
        data_pollen[["percentages_harmonised"]],
        is.data.frame
      )
    ),
    all(
      purrr::map_lgl(
        data_pollen[["levels"]],
        is.data.frame
      )
    ),
    is.numeric(n_rand),
    length(n_rand) == 1L,
    is.finite(n_rand),
    n_rand > 0,
    is.character(transformation_coef),
    length(transformation_coef) == 1L,
    is.function(fit_dataset),
    msg = paste(
      "MRT dataset inputs must be tables and `n_rand` must be",
      "a positive scalar."
    )
  )

  list_mrt <-
    purrr::map2(
      .x = data_pollen[["percentages_harmonised"]],
      .y = data_pollen[["levels"]],
      .f = ~ fit_dataset(
        data_source_counts = .x,
        data_source_levels = .y,
        n_rand = n_rand,
        transformation = transformation_coef
      )
    )

  data_mrt <-
    data.frame(
      dataset_id = data_pollen[["dataset_id"]],
      stringsAsFactors = FALSE
    )

  data_mrt[["mvrt_partitions"]] <-
    I(
      purrr::map(
        list_mrt,
        ~ .x[["partitions"]]
      )
    )

  data_mrt[["mvrt_cp"]] <-
    I(
      purrr::map(
        list_mrt,
        ~ .x[["change_points"]]
      )
    )

  data_mrt[["mvrt_groups_n"]] <-
    purrr::map_dbl(
      list_mrt,
      ~ .x[["mrt_groups"]]
    )

  return(data_mrt)
}
