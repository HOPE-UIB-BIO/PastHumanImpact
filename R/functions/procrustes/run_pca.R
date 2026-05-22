#' @title A wrapper function to run Principal Canonical Analysis
#' @param x Input response data as a data frame containing PAP columns.
#' @param scale Logical; passed to `vegan::rda()`.
#' @return A `vegan::rda` object.

run_pca <- function(x, scale = TRUE) {
  assertthat::assert_that(
    is.data.frame(x),
    msg = "`x` must be a data frame."
  )
  assertthat::assert_that(
    is.logical(scale) && length(scale) == 1,
    msg = "`scale` must be a single logical value."
  )
  assertthat::assert_that(
    all(c(
      "n0", "n1", "n2", "n1_minus_n2", "n2_divided_by_n1",
      "n1_divided_by_n0", "roc", "dcca_axis_1", "density_diversity"
    ) %in% names(x)),
    msg = "`x` must contain PAP columns from `n0` to `density_diversity`."
  )

  resp <-
    x %>%
    dplyr::select(n0:density_diversity)

  mod <-
    vegan::rda(resp,
      scale = scale,
      data = x
    )

  return(mod)
}
