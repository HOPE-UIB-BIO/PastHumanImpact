#' @title Run dbRDA on m2 distances and predictors
#' @description
#' Runs `vegan::dbrda()` when both distance and predictor inputs are present.
#' Returns `NULL` when either input is `NULL`.
#' @param data_m2 Matrix of pairwise m2 distances, or `NULL`.
#' @param data_pred Predictor data frame containing an `age` column, or `NULL`.
#' @return A `vegan::dbrda` object, or `NULL` if either input is `NULL`.

run_dbrda <- function(data_m2, data_pred) {
  if (!is.null(data_m2) & !is.null(data_pred)) {
    assertthat::assert_that(
      is.matrix(data_m2),
      msg = "`data_m2` must be a matrix when provided."
    )
    assertthat::assert_that(
      nrow(data_m2) == ncol(data_m2),
      msg = "`data_m2` must be a square matrix when provided."
    )
    assertthat::assert_that(
      is.data.frame(data_pred),
      msg = "`data_pred` must be a data frame when provided."
    )
    assertthat::assert_that(
      "age" %in% names(data_pred),
      msg = "`data_pred` must contain an `age` column."
    )

    # convert m2 distances to euclidean distances
    data_dist <-
      stats::as.dist(data_m2)

    preds <-
      data_pred %>%
      dplyr::select(-age)

    # run dbRDA
    dbRDA_res <-
      vegan::dbrda(
        formula = data_dist ~ .,
        scale = TRUE,
        data = preds
      )
    return(dbRDA_res)
  } else {
    return(NULL)
  }
}
