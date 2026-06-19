#' @title Predict marginal response from fitted model
#' @description
#' Builds age-wise marginal predictions. When `newdata` is supplied for a
#' `brmsfit`, predictions are extracted with `brms::fitted()` on the response
#' scale. Otherwise the legacy `ggeffects::predict_response()` path is used.
#' @param mod Fitted model object accepted by `ggeffects::predict_response()`.
#' @param newdata Optional prediction data for fitted `brmsfit` models.
#' @param model_config_row Optional one-row model configuration data frame used
#' to append `analysis`, `model_id`, and `variable`.
#' @return Data frame with columns `age`, `value`, and confidence bounds.
predict_brms_model <- function(mod, newdata = NULL, model_config_row = NULL) {
  assertthat::assert_that(
    !is.null(mod),
    msg = "`mod` must not be NULL."
  )
  assertthat::assert_that(
    !is.atomic(mod),
    msg = "`mod` must be a fitted model object."
  )

  if (
    isFALSE(is.null(newdata))
  ) {
    assertthat::assert_that(
      inherits(mod, "brmsfit"),
      msg = "`mod` must be a fitted brms model when `newdata` is supplied."
    )
    assertthat::assert_that(
      is.data.frame(newdata),
      msg = "`newdata` must be a data frame."
    )
    assertthat::assert_that(
      is.null(model_config_row) ||
        (is.data.frame(model_config_row) && nrow(model_config_row) == 1),
      msg = "`model_config_row` must be NULL or a one-row data frame."
    )

    data_predicted <-
      stats::fitted(
        object = mod,
        newdata = newdata,
        re_formula = NA,
        scale = "response",
        allow_new_levels = TRUE,
        summary = TRUE
      ) %>%
      as.data.frame() %>%
      janitor::clean_names()

    res_data <-
      dplyr::bind_cols(
        newdata,
        data_predicted
      ) %>%
      dplyr::rename(
        estimate = estimate,
        estimate_error = est_error,
        conf_low = q2_5,
        conf_high = q97_5
      )

    if (
      isFALSE(is.null(model_config_row))
    ) {
      res_data <-
        res_data %>%
        dplyr::mutate(
          analysis = model_config_row[["analysis"]][1],
          model_id = model_config_row[["model_id"]][1],
          variable = model_config_row[["variable"]][1]
        )
    }

    return(res_data)
  }

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
