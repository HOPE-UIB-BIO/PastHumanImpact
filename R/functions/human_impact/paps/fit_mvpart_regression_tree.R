#' @title Fit one legacy regression tree
#' @description
#' Fit and prune one univariate regression tree with the archived `mvpart`
#' implementation and return its change points.
#' @param data_source Data frame containing response and age columns.
#' @param response_name Character scalar response-column name.
#' @param age_name Character scalar age-column name.
#' @param fit_backend Optional function used to fit the regression tree.
#' @param prune_backend Optional function used to prune the fitted tree.
#' @param summary_backend Function used to summarise the pruned tree.
#' @return A numeric vector of change-point ages.
#' @examples
#' \dontrun{
#' fit_mvpart_regression_tree(data_source, "value", "age")
#' }
fit_mvpart_regression_tree <- function(
    data_source,
    response_name,
    age_name,
    fit_backend = NULL,
    prune_backend = NULL,
    summary_backend = summary
) {
  if (
    is.null(fit_backend)
  ) {
    fit_backend <-
      getExportedValue(
        name = "rpart",
        ns = "mvpart"
      )
  }

  if (
    is.null(prune_backend)
  ) {
    prune_backend <-
      getExportedValue(
        name = "prune",
        ns = "mvpart"
      )
  }

  assertthat::assert_that(
    is.data.frame(data_source),
    is.character(response_name),
    length(response_name) == 1L,
    is.character(age_name),
    length(age_name) == 1L,
    all(c(response_name, age_name) %in% names(data_source)),
    is.function(fit_backend),
    is.function(prune_backend),
    is.function(summary_backend),
    msg = "Regression-tree fitting arguments are invalid."
  )

  formula_model <-
    stats::reformulate(
      termlabels = age_name,
      response = response_name
    )

  mod_tree <-
    fit_backend(
      formula = formula_model,
      method = "anova",
      data = data_source
    )

  table_complexity <-
    mod_tree[["cptable"]]

  assertthat::assert_that(
    is.matrix(table_complexity),
    all(c("CP", "xerror") %in% colnames(table_complexity)),
    msg = "The fitted regression tree has an invalid complexity table."
  )

  value_cp <-
    if (
      nrow(table_complexity) > 1L
    ) {
      table_complexity[which.min(table_complexity[, "xerror"]), "CP"]
    } else {
      0
    }

  mod_pruned <-
    prune_backend(
      tree = mod_tree,
      cp = value_cp
    )

  vec_change_points <-
    compute_mvpart_change_points(
      model = mod_pruned,
      summary_backend = summary_backend
    )

  return(vec_change_points)
}
