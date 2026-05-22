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
#' @param n_groups Integer count of groups for random-effect smooth.
#' @param common_trend Logical. If `TRUE`, include common trend term.
#' @return Character scalar formula.
get_hgam_formula <- function(
  x_var = "age",
  y_var = "var",
  group_var = "dataset_id",
  smooth_basis = c("tp", "cr"),
  sel_k = 10,
  sel_m = NULL,
  n_groups,
  common_trend = TRUE
) {
  RUtilpol::check_class("y_var", "character")

  RUtilpol::check_class("x_var", "character")

  RUtilpol::check_class("group_var", "character")

  smooth_basis <- match.arg(smooth_basis)

  RUtilpol::check_class("smooth_basis", "character")

  RUtilpol::check_vector_values("smooth_basis", c("tp", "cr"))

  RUtilpol::check_class("sel_k", "numeric")

  assertthat::assert_that(
    assertthat::is.count(sel_k),
    msg = "'sel_k' must be an integer"
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

  assertthat::assert_that(
    assertthat::is.count(n_groups),
    msg = "'n_groups' must be an integer"
  )

  formula_gam <-
    paste0(
      y_var,
      " ~ s(",
      x_var,
      ", k = ", sel_k,
      ", bs = '", smooth_basis, "'",
      ")"
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

  return(formula_hgam_fin)
}
