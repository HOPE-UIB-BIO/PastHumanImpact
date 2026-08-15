#' @title Transform pollen composition
#' @description
#' Apply the requested legacy MRT transformation to percentage-valued pollen
#' data while retaining sample identifiers.
#' @param data_percentages Data frame containing `sample_id` and numeric taxa.
#' @param transformation One of `chisq`, `hellinger`, or `none`.
#' @return A data frame containing `sample_id` and transformed taxa.
#' @examples
#' prepare_transformed_pollen_composition(
#'   data.frame(sample_id = c("a", "b"), taxon = c(100, 100)),
#'   transformation = "none"
#' )
prepare_transformed_pollen_composition <- function(
    data_percentages,
    transformation = c("chisq", "hellinger", "none")
) {
  assertthat::assert_that(
    is.data.frame(data_percentages),
    "sample_id" %in% names(data_percentages),
    is.character(transformation),
    length(transformation) >= 1L,
    msg = "Pollen transformation arguments are invalid."
  )

  transformation <-
    match.arg(transformation)

  vec_taxa <-
    setdiff(
      names(data_percentages),
      "sample_id"
    )

  mat_percentages <-
    data.matrix(data_percentages[vec_taxa])

  if (
    identical(transformation, "chisq")
  ) {
    mat_transformed <-
      compute_chi_square_standardisation(mat_percentages)
  } else if (
    identical(transformation, "hellinger")
  ) {
    mat_transformed <-
      sqrt(
        sweep(
          mat_percentages,
          MARGIN = 1L,
          STATS = rowSums(mat_percentages),
          FUN = "/"
        )
      )
  } else {
    mat_transformed <- mat_percentages
  }

  data_transformed <-
    data.frame(
      sample_id = data_percentages[["sample_id"]],
      mat_transformed,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

  return(data_transformed)
}
