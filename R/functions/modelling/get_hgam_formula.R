#' @title Build HGAM formula string
#' @description
#' Build a GAM/HGAM formula string for grouped trend modelling with optional
#' common trend term.
#' @param x_var Character scalar predictor variable name.
#' @param y_var Character scalar response variable name.
#' @param group_var Character scalar grouping variable name.
#' @param smooth_basis One of `tp` or `cr`.
#' @param sel_k Integer smooth basis dimension.
#' @param sel_m Optional integer smooth penalty order. If `NULL`, derived from
#' `common_trend`.
#' @param n_groups Integer count of groups for random-effect smooth in the
#' dataset-level profile.
#' @param common_trend Logical. If `TRUE`, include common trend term.
#' @param model_profile Formula profile to build. `dataset_smooth` preserves
#' the historical formula. `stratum_fs` uses a random factor smooth by stratum
#' plus a dataset random intercept. `stratum_by` is a non-`fs` comparator.
#' `stratum_fs_dataset_slope` adds a dataset-specific linear age effect.
#' `stratum_fs_dataset_fs` adds dataset-specific factor smooths.
#' `within_stratum_dataset_intercept`, `within_stratum_dataset_slope`,
#' `within_stratum_dataset_slope_uncorrelated`, and
#' `within_stratum_dataset_fs` are profiles for separate models fitted within
#' one region-climatezone stratum. The uncorrelated slope profile estimates
#' dataset intercept and slope variation without their correlation.
#' @param stratum_var Character scalar stratum variable name.
#' @param stratum_k Integer basis dimension for stratum smooths.
#' @param group_k Integer basis dimension for dataset-level factor smooths.
#' @return Character scalar formula.
get_hgam_formula <- function(
  x_var = "age",
  y_var = "var",
  group_var = "dataset_id",
  smooth_basis = c("cr", "tp"),
  sel_k = 10,
  sel_m = NULL,
  n_groups = NULL,
  common_trend = TRUE,
  model_profile = c(
    "dataset_smooth",
    "stratum_fs",
    "stratum_by",
    "stratum_fs_dataset_slope",
    "stratum_fs_dataset_fs",
    "within_stratum_dataset_intercept",
    "within_stratum_dataset_slope",
    "within_stratum_dataset_slope_uncorrelated",
    "within_stratum_dataset_fs"
  ),
  stratum_var = "stratum",
  stratum_k = 5,
  group_k = 3
) {
  model_profile <- match.arg(model_profile)

  RUtilpol::check_class("y_var", "character")

  RUtilpol::check_class("x_var", "character")

  RUtilpol::check_class("group_var", "character")

  RUtilpol::check_class("stratum_var", "character")

  smooth_basis <- match.arg(smooth_basis)

  RUtilpol::check_class("smooth_basis", "character")

  RUtilpol::check_vector_values("smooth_basis", c("tp", "cr"))

  RUtilpol::check_class("sel_k", "numeric")

  assertthat::assert_that(
    assertthat::is.count(sel_k),
    msg = "'sel_k' must be an integer"
  )

  RUtilpol::check_class("stratum_k", "numeric")

  assertthat::assert_that(
    assertthat::is.count(stratum_k),
    msg = "'stratum_k' must be an integer"
  )

  RUtilpol::check_class("group_k", "numeric")

  assertthat::assert_that(
    assertthat::is.count(group_k),
    msg = "'group_k' must be an integer"
  )

  RUtilpol::check_class("sel_m", c("NULL", "numeric"))

  if (
    isFALSE(is.null(sel_m))
  ) {
    assertthat::assert_that(
      assertthat::is.count(sel_m),
      msg = "'sel_m' must be an integer"
    )
  }
  RUtilpol::check_class("common_trend", "logical")

  if (
    is.null(sel_m)
  ) {
    sel_m <-
      ifelse(common_trend, 1, 2)
  }

  formula_gam <-
    paste0(
      y_var,
      " ~ s(",
      x_var,
      ", k = ", sel_k,
      ", bs = '", smooth_basis, "'",
      ")"
    )

  if (model_profile == "dataset_smooth") {
    assertthat::assert_that(
      assertthat::is.count(n_groups),
      msg = "'n_groups' must be an integer"
    )

    formula_hgam <-
      paste(
        paste0(
          "s(", x_var,
          ", by = ", group_var,
          ", bs = '", smooth_basis, "'",
          ", k = ", sel_k,
          ", m = ", sel_m,
          ")"
        ),
        paste0(
          "s(", group_var,
          ", bs = 're'",
          ", k = ", n_groups,
          ")"
        ),
        sep = " + "
      )

    if (
      isTRUE(common_trend)
    ) {
      formula_hgam_fin <-
        paste(
          formula_gam,
          formula_hgam,
          sep = " + "
        )
    } else {
      formula_hgam_fin <-
        paste0(y_var, " ~ ", formula_hgam)
    }
  } else if (model_profile == "stratum_fs") {
    formula_hgam <-
      paste(
        paste0(
          "s(", x_var,
          ", ", stratum_var,
          ", bs = 'fs'",
          ", k = ", stratum_k,
          ")"
        ),
        paste0("(1 | ", group_var, ")"),
        sep = " + "
      )

    if (
      isTRUE(common_trend)
    ) {
      formula_hgam_fin <-
        paste(
          formula_gam,
          formula_hgam,
          sep = " + "
        )
    } else {
      formula_hgam_fin <-
        paste0(y_var, " ~ ", formula_hgam)
    }
  } else if (model_profile == "stratum_by") {
    formula_hgam <-
      paste(
        paste0(
          "s(", x_var,
          ", by = ", stratum_var,
          ", bs = '", smooth_basis, "'",
          ", k = ", stratum_k,
          ", m = ", sel_m,
          ")"
        ),
        paste0("(1 | ", group_var, ")"),
        sep = " + "
      )

    if (
      isTRUE(common_trend)
    ) {
      formula_hgam_fin <-
        paste(
          formula_gam,
          formula_hgam,
          sep = " + "
        )
    } else {
      formula_hgam_fin <-
        paste0(y_var, " ~ ", formula_hgam)
    }
  } else if (model_profile == "stratum_fs_dataset_slope") {
    formula_hgam <-
      paste(
        paste0(
          "s(", x_var,
          ", ", stratum_var,
          ", bs = 'fs'",
          ", k = ", stratum_k,
          ")"
        ),
        paste0("(1 + ", x_var, " | ", group_var, ")"),
        sep = " + "
      )

    if (
      isTRUE(common_trend)
    ) {
      formula_hgam_fin <-
        paste(
          formula_gam,
          formula_hgam,
          sep = " + "
        )
    } else {
      formula_hgam_fin <-
        paste0(y_var, " ~ ", formula_hgam)
    }
  } else if (model_profile == "stratum_fs_dataset_fs") {
    formula_hgam <-
      paste(
        paste0(
          "s(", x_var,
          ", ", stratum_var,
          ", bs = 'fs'",
          ", k = ", stratum_k,
          ")"
        ),
        paste0(
          "s(", x_var,
          ", ", group_var,
          ", bs = 'fs'",
          ", k = ", group_k,
          ")"
        ),
        sep = " + "
      )

    if (
      isTRUE(common_trend)
    ) {
      formula_hgam_fin <-
        paste(
          formula_gam,
          formula_hgam,
          sep = " + "
        )
    } else {
      formula_hgam_fin <-
        paste0(y_var, " ~ ", formula_hgam)
    }
  } else if (model_profile == "within_stratum_dataset_intercept") {
    formula_hgam <-
      paste0("(1 | ", group_var, ")")

    if (
      isTRUE(common_trend)
    ) {
      formula_hgam_fin <-
        paste(
          formula_gam,
          formula_hgam,
          sep = " + "
        )
    } else {
      formula_hgam_fin <-
        paste0(y_var, " ~ ", formula_hgam)
    }
  } else if (model_profile == "within_stratum_dataset_slope") {
    formula_hgam <-
      paste0("(1 + ", x_var, " | ", group_var, ")")

    if (
      isTRUE(common_trend)
    ) {
      formula_hgam_fin <-
        paste(
          formula_gam,
          formula_hgam,
          sep = " + "
        )
    } else {
      formula_hgam_fin <-
        paste0(y_var, " ~ ", formula_hgam)
    }
  } else if (
    model_profile == "within_stratum_dataset_slope_uncorrelated"
  ) {
    formula_hgam <-
      paste0("(1 + ", x_var, " || ", group_var, ")")

    if (
      isTRUE(common_trend)
    ) {
      formula_hgam_fin <-
        paste(
          formula_gam,
          formula_hgam,
          sep = " + "
        )
    } else {
      formula_hgam_fin <-
        paste0(y_var, " ~ ", formula_hgam)
    }
  } else if (model_profile == "within_stratum_dataset_fs") {
    formula_hgam <-
      paste0(
        "s(", x_var,
        ", ", group_var,
        ", bs = 'fs'",
        ", k = ", group_k,
        ")"
      )

    if (
      isTRUE(common_trend)
    ) {
      formula_hgam_fin <-
        paste(
          formula_gam,
          formula_hgam,
          sep = " + "
        )
    } else {
      formula_hgam_fin <-
        paste0(y_var, " ~ ", formula_hgam)
    }
  }

  return(formula_hgam_fin)
}
