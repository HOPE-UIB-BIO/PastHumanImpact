#' @title Fit one spatial importance aggregation model
#' @description
#' Fit a weighted linear model with complete spatial strata and selected dbMEM
#' terms for one core-level importance profile.
#' @param data_model Model data containing `spatial_stratum`.
#' @param response_col Response column name.
#' @param weight_col Weight column name.
#' @param selected_mem_names Selected dbMEM column names.
#' @return A fitted `lm` object.
#' @examples
#' \dontrun{
#' fit_spatial_importance_profile(
#'   data_model = records,
#'   response_col = "signed_balance",
#'   weight_col = "signed_weight",
#'   selected_mem_names = "dbmem_001"
#' )
#' }
fit_spatial_importance_profile <- function(
  data_model,
  response_col,
  weight_col,
  selected_mem_names = character()
) {
  assertthat::assert_that(
    is.data.frame(data_model),
    assertthat::is.string(response_col),
    assertthat::is.string(weight_col),
    is.character(selected_mem_names),
    all(c(
      "spatial_stratum",
      response_col,
      weight_col,
      selected_mem_names
    ) %in% names(data_model)),
    msg = "Spatial importance model inputs do not satisfy the contract."
  )

  vec_terms <- c("0 + spatial_stratum", selected_mem_names)
  model_formula <-
    stats::as.formula(
      stringr::str_c(
        response_col,
        " ~ ",
        stringr::str_c(vec_terms, collapse = " + ")
      )
    )
  res_model <-
    stats::lm(
      formula = model_formula,
      data = data_model,
      weights = data_model[[weight_col]],
      na.action = stats::na.exclude
    )

  return(res_model)
}
