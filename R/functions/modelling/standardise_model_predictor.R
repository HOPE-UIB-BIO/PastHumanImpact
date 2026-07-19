#' @title Standardise a model predictor
#' @description
#' Add a standardised predictor column using stored model-specific scaling
#' statistics while retaining the original predictor.
#' @param data_source Data frame containing the original predictor.
#' @param x_var Name of the original predictor column.
#' @param x_model_var Name of the standardised predictor column to create.
#' @param x_mean Numeric predictor mean from the model fitting subset.
#' @param x_sd Numeric predictor standard deviation from the fitting subset.
#' @return Data frame with `x_model_var` added.
#' @examples
#' data_scaled <- standardise_model_predictor(
#'   data_source = data.frame(age_ka = c(0, 1, 2)),
#'   x_var = "age_ka",
#'   x_model_var = "age_ka_scaled",
#'   x_mean = 1,
#'   x_sd = 1
#' )
standardise_model_predictor <- function(
  data_source,
  x_var,
  x_model_var,
  x_mean,
  x_sd
) {
  assertthat::assert_that(
    is.data.frame(data_source),
    msg = "`data_source` must be a data frame."
  )
  assertthat::assert_that(
    is.character(x_var),
    length(x_var) == 1,
    is.character(x_model_var),
    length(x_model_var) == 1,
    x_var != x_model_var,
    msg = "Predictor names must be distinct character scalars."
  )
  assertthat::assert_that(
    x_var %in% names(data_source),
    msg = "`data_source` must contain `x_var`."
  )
  assertthat::assert_that(
    is.numeric(x_mean),
    length(x_mean) == 1,
    is.finite(x_mean),
    is.numeric(x_sd),
    length(x_sd) == 1,
    is.finite(x_sd),
    x_sd > 0,
    msg = "`x_mean` must be finite and `x_sd` must be finite and positive."
  )
  assertthat::assert_that(
    all(is.finite(data_source[[x_var]])),
    msg = "The model predictor must contain only finite values."
  )

  res_data <-
    data_source %>%
    dplyr::mutate(
      "{x_model_var}" := (.data[[x_var]] - x_mean) / x_sd
    )

  return(res_data)
}
