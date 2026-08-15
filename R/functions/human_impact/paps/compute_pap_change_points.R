#' @title Compute pollen-property change points
#' @description
#' Fit the univariate legacy regression trees for every prepared pollen
#' dataset and assemble their change-point summaries.
#' @param data_source Data frame containing prepared pollen-property
#'   list-columns.
#' @param fit_dataset Function used to fit one prepared dataset.
#' @return A data frame with dataset-level MRT, diversity, ROC, peak, and DCCA
#'   change points.
#' @details
#' The underlying `mvpart` package requires the isolated old-R runtime
#' documented in `R/analyses/01_data_preparation/05_paps/README.md`. Do not
#' source the project-wide configuration from that runtime.
#' @examples
#' \dontrun{
#' compute_pap_change_points(data_source)
#' }
compute_pap_change_points <- function(
    data_source,
    fit_dataset = fit_pap_change_point_dataset
) {
  vec_required_columns <-
    c(
      "dataset_id",
      "mvrt_cp",
      "PAP_diversity",
      "levels",
      "PAP_roc",
      "dcca_scores"
    )

  assertthat::assert_that(
    is.data.frame(data_source),
    all(vec_required_columns %in% names(data_source)),
    is.function(fit_dataset),
    msg = paste(
      "`data_source` is missing required columns for",
      "change-point estimation."
    )
  )

  vec_nested_columns <-
    c(
      "PAP_diversity",
      "levels",
      "PAP_roc",
      "dcca_scores"
    )

  assertthat::assert_that(
    all(
      purrr::map_lgl(
        vec_nested_columns,
        ~ all(
          purrr::map_lgl(
            data_source[[.x]],
            is.data.frame
          )
        )
      )
    ),
    msg = paste(
      "PAP_diversity, levels, PAP_roc, and dcca_scores",
      "list-columns must contain data frames."
    )
  )

  list_change_points <-
    purrr::pmap(
      .l = list(
        data_source[["mvrt_cp"]],
        data_source[["PAP_diversity"]],
        data_source[["levels"]],
        data_source[["PAP_roc"]],
        data_source[["dcca_scores"]]
      ),
      .f = ~ fit_dataset(
        mvrt_cp = ..1,
        data_diversity = ..2,
        data_levels = ..3,
        data_roc = ..4,
        data_dcca = ..5
      )
    )

  data_change_points <-
    data.frame(
      dataset_id = data_source[["dataset_id"]],
      stringsAsFactors = FALSE
    )

  data_change_points[["mvrt_cp"]] <-
    I(
      purrr::map(
        list_change_points,
        ~ .x[["mvrt_cp"]]
      )
    )

  data_change_points[["diversity_cp"]] <-
    I(
      purrr::map(
        list_change_points,
        ~ .x[["diversity_cp"]]
      )
    )

  data_change_points[["roc_cp"]] <-
    I(
      purrr::map(
        list_change_points,
        ~ .x[["roc_cp"]]
      )
    )

  data_change_points[["roc_pp"]] <-
    I(
      purrr::map(
        list_change_points,
        ~ .x[["roc_pp"]]
      )
    )

  data_change_points[["dcca_cp"]] <-
    I(
      purrr::map(
        list_change_points,
        ~ .x[["dcca_cp"]]
      )
    )

  return(data_change_points)
}
