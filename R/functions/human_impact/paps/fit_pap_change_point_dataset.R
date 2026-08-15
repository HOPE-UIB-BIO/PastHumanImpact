#' @title Fit PAP change points for one dataset
#' @description
#' Fit all univariate legacy regression trees required for one pollen dataset.
#' @param mvrt_cp Numeric multivariate-tree change points.
#' @param data_diversity Diversity table containing `sample_id`.
#' @param data_levels Level table containing `sample_id` and `age`.
#' @param data_roc Rate-of-change table containing `Age`, `ROC`, and `Peak`.
#' @param data_dcca DCCA table containing `sample_id` and `axis_1`.
#' @param fit_backend Optional function used to fit each regression tree.
#' @param prune_backend Optional function used to prune each regression tree.
#' @param summary_backend Function used to summarise each regression tree.
#' @return A list of multivariate, diversity, ROC, peak, and DCCA change points.
#' @examples
#' \dontrun{
#' fit_pap_change_point_dataset(
#'   numeric(0), data_diversity, data_levels, data_roc, data_dcca
#' )
#' }
fit_pap_change_point_dataset <- function(
    mvrt_cp,
    data_diversity,
    data_levels,
    data_roc,
    data_dcca,
    fit_backend = NULL,
    prune_backend = NULL,
    summary_backend = summary
) {
  assertthat::assert_that(
    is.numeric(mvrt_cp),
    is.data.frame(data_diversity),
    is.data.frame(data_levels),
    is.data.frame(data_roc),
    is.data.frame(data_dcca),
    "sample_id" %in% names(data_diversity),
    all(c("sample_id", "age") %in% names(data_levels)),
    all(c("Age", "ROC", "Peak") %in% names(data_roc)),
    all(c("sample_id", "axis_1") %in% names(data_dcca)),
    is.null(fit_backend) || is.function(fit_backend),
    is.null(prune_backend) || is.function(prune_backend),
    is.function(summary_backend),
    msg = "PAP change-point inputs are invalid."
  )

  vec_diversity_names <-
    setdiff(
      names(data_diversity),
      "sample_id"
    )

  vec_diversity_ages <-
    data_levels[["age"]][
      match(
        data_diversity[["sample_id"]],
        data_levels[["sample_id"]]
      )
    ]

  assertthat::assert_that(
    !anyNA(vec_diversity_ages),
    msg = "Diversity sample identifiers are absent from the level table."
  )

  list_diversity_cp <-
    purrr::map(
      .x = vec_diversity_names,
      .f = ~ fit_mvpart_regression_tree(
        data_source = data.frame(
          value = data_diversity[[.x]],
          age = vec_diversity_ages
        ),
        response_name = "value",
        age_name = "age",
        fit_backend = fit_backend,
        prune_backend = prune_backend,
        summary_backend = summary_backend
      )
    )

  data_diversity_cp <-
    data.frame(
      var_name = rep(
        vec_diversity_names,
        lengths(list_diversity_cp)
      ),
      age = unlist(
        list_diversity_cp,
        use.names = FALSE
      ),
      stringsAsFactors = FALSE
    )

  vec_roc_cp <-
    fit_mvpart_regression_tree(
      data_source = data_roc,
      response_name = "ROC",
      age_name = "Age",
      fit_backend = fit_backend,
      prune_backend = prune_backend,
      summary_backend = summary_backend
    )

  vec_dcca_ages <-
    data_levels[["age"]][
      match(
        data_dcca[["sample_id"]],
        data_levels[["sample_id"]]
      )
    ]

  assertthat::assert_that(
    !anyNA(vec_dcca_ages),
    msg = "DCCA sample identifiers are absent from the level table."
  )

  vec_dcca_cp <-
    fit_mvpart_regression_tree(
      data_source = data.frame(
        axis_1 = data_dcca[["axis_1"]],
        age = vec_dcca_ages
      ),
      response_name = "axis_1",
      age_name = "age",
      fit_backend = fit_backend,
      prune_backend = prune_backend,
      summary_backend = summary_backend
    )

  res_change_points <-
    list(
      mvrt_cp = mvrt_cp,
      diversity_cp = data_diversity_cp,
      roc_cp = vec_roc_cp,
      roc_pp = data_roc[["Age"]][data_roc[["Peak"]] %in% TRUE],
      dcca_cp = vec_dcca_cp
    )

  return(res_change_points)
}
