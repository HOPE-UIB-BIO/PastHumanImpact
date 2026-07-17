#' @title Predict marginal response from fitted model
#' @description
#' Builds age-wise marginal predictions. When `newdata` is supplied for a
#' `brmsfit`, posterior expected responses are averaged equally across datasets
#' within each posterior draw. Otherwise the legacy
#' `ggeffects::predict_response()` path is used.
#' @param mod Fitted model object accepted by `ggeffects::predict_response()`.
#' @param newdata Optional prediction data for fitted `brmsfit` models.
#' @param model_config_row Optional one-row model configuration data frame used
#' to append `analysis`, `model_id`, and `variable`.
#' @param max_prediction_draws Maximum number of evenly spaced posterior draws
#' used for marginal predictions.
#' @return Data frame with age, posterior estimate, uncertainty bounds, and
#' prediction provenance columns.
#' @details
#' Dataset-specific smooths and random effects are included. Expected responses
#' are averaged with equal dataset weights within each posterior draw, so
#' uncertainty intervals describe the posterior dataset-mean trajectory.
predict_brms_model <- function(
  mod,
  newdata = NULL,
  model_config_row = NULL,
  max_prediction_draws = 1000L
) {
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
      is.data.frame(model_config_row),
      nrow(model_config_row) == 1L,
      msg = "`model_config_row` must be a one-row data frame."
    )
    assertthat::assert_that(
      all(
        c(
          "analysis",
          "model_id",
          "variable",
          "group_var",
          "model_profile",
          "model_file_name"
        ) %in% names(model_config_row)
      ),
      msg = "`model_config_row` is missing prediction metadata columns."
    )
    assertthat::assert_that(
      assertthat::is.count(max_prediction_draws),
      msg = "`max_prediction_draws` must be a positive integer."
    )

    n_available_draws <-
      posterior::ndraws(mod)
    n_prediction_draws <-
      min(max_prediction_draws, n_available_draws)
    prediction_draw_ids <-
      seq(
        from = 1,
        to = n_available_draws,
        length.out = n_prediction_draws
      ) %>%
      round() %>%
      unique()

    mat_expected_response <-
      brms::posterior_epred(
        object = mod,
        newdata = newdata,
        re_formula = NULL,
        allow_new_levels = FALSE,
        draw_ids = prediction_draw_ids
      )

    res_data <-
      summarise_prediction_draws(
        mat_draws = mat_expected_response,
        data_new = newdata,
        group_var = model_config_row[["group_var"]][1]
      ) %>%
      dplyr::mutate(
        analysis = model_config_row[["analysis"]][1],
        model_id = model_config_row[["model_id"]][1],
        variable = model_config_row[["variable"]][1],
        model_profile = model_config_row[["model_profile"]][1],
        source_model_file = model_config_row[["model_file_name"]][1],
        prediction_draws_used = length(prediction_draw_ids),
        prediction_estimand = "equal_weighted_dataset_mean"
      )

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

  return(data_predicted)
}
