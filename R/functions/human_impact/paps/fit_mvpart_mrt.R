#' @title Fit one multivariate regression tree
#' @description
#' Prepare one pollen dataset, fit the archived multivariate regression tree,
#' and return only stable plain-data summaries.
#' @param data_source_counts Data frame containing `sample_id` and pollen taxa.
#' @param data_source_levels Data frame containing `sample_id` and `age`.
#' @param n_rand Positive number of cross-validation repetitions.
#' @param transformation One of `chisq`, `hellinger`, or `none`.
#' @param fit_backend Optional function used to fit the multivariate tree.
#' @param summary_backend Function used to summarise the fitted tree.
#' @return A list with `partitions`, `change_points`, and `mrt_groups`.
#' @examples
#' \dontrun{
#' fit_mvpart_mrt(data_source_counts, data_source_levels)
#' }
fit_mvpart_mrt <- function(
    data_source_counts,
    data_source_levels,
    n_rand = 999,
    transformation = "chisq",
    fit_backend = NULL,
    summary_backend = summary
) {
  if (
    is.null(fit_backend)
  ) {
    fit_backend <-
      getExportedValue(
        name = "mvpart",
        ns = "mvpart"
      )
  }

  assertthat::assert_that(
    is.data.frame(data_source_levels),
    all(c("sample_id", "age") %in% names(data_source_levels)),
    is.numeric(n_rand),
    length(n_rand) == 1L,
    is.finite(n_rand),
    n_rand > 0,
    is.function(fit_backend),
    msg = "MRT fitting arguments are invalid."
  )

  data_percentages <-
    prepare_pollen_percentages(data_source_counts)

  data_transformed <-
    prepare_transformed_pollen_composition(
      data_percentages = data_percentages,
      transformation = transformation
    )

  assertthat::assert_that(
    identical(
      data_transformed[["sample_id"]],
      data_source_levels[["sample_id"]]
    ),
    msg = "Pollen counts and levels must have identical sample order."
  )

  mat_response <-
    data.matrix(
      data_transformed[
        setdiff(names(data_transformed), "sample_id")
      ]
    )

  age <-
    data_source_levels[["age"]]

  mod_mrt <-
    fit_backend(
      form = mat_response ~ age,
      xv = "1se",
      xvmult = n_rand,
      plot.add = FALSE,
      data = data.frame(age = age)
    )

  assertthat::assert_that(
    !is.null(mod_mrt[["where"]]),
    length(mod_mrt[["where"]]) == nrow(data_source_levels),
    msg = "The fitted MRT result has invalid terminal-node assignments."
  )

  vec_groups <-
    normalise_partition_groups(mod_mrt[["where"]])

  data_partitions <-
    data.frame(
      sample_id = data_source_levels[["sample_id"]],
      MRT_partitions = vec_groups,
      stringsAsFactors = FALSE
    )

  res_mrt <-
    list(
      partitions = data_partitions,
      change_points = compute_mvpart_change_points(
        model = mod_mrt,
        summary_backend = summary_backend
      ),
      mrt_groups = length(unique(vec_groups))
    )

  return(res_mrt)
}
