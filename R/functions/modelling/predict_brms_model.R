#' @title Predict marginal response from fitted model
#' @description
#' Builds age-wise marginal predictions using `ggeffects::predict_response()`
#' and applies extra exponentiation when the model link is `log`.
#' @param mod Fitted model object accepted by `ggeffects::predict_response()`.
#' @return Data frame with columns `age`, `value`, and confidence bounds.
predict_brms_model <- function(mod) {
  assertthat::assert_that(
    !is.null(mod),
    msg = "`mod` must not be NULL."
  )
  assertthat::assert_that(
    !is.atomic(mod),
    msg = "`mod` must be a fitted model object."
  )

  data_predicted <-
    ggeffects::predict_response(
      model = mod,
      terms = "age",
      margin = "marginalmeans",
      back_transform = TRUE
    ) %>%
    as.data.frame() %>%
    dplyr::rename(
      age = x,
      value = predicted
    )

  # back transform if needed -----
  sel_family <-
    insight::get_family(mod)

  sel_link <-
    sel_family %>%
    purrr::pluck("link") %>%
    as.character()

  if (
    sel_link == "log"
  ) {
    data_predicted <-
      data_predicted %>%
      dplyr::mutate(
        value = exp(value),
        conf.low = exp(conf.low),
        conf.high = exp(conf.high)
      )
  }
  return(data_predicted)
}
