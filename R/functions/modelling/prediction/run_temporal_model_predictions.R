#' @title Run temporal-model predictions
#' @description
#' Reuse valid cached temporal predictions and regenerate only predictions that
#' are missing or stale relative to evaluated fitted models.
#' @param data_source Temporal model input data.
#' @param config_dir Existing temporal-model configuration directory.
#' @param model_dir Existing fitted-model directory.
#' @param prediction_dir Existing or creatable prediction directory.
#' @param rewrite Logical scalar forcing prediction regeneration.
#' @param max_prediction_draws Positive integer maximum posterior draws.
#' @param prediction_range Prediction support policy.
#' @param verbose Logical scalar controlling progress messages.
#' @return Combined prediction tibble for all eligible ready models.
#' @examples
#' \dontrun{
#' predictions <- run_temporal_model_predictions(
#'   data_source = model_data,
#'   config_dir = "Data/Temporal_models",
#'   model_dir = "Data/Temporal_models/Mods",
#'   prediction_dir = "Data/Temporal_models/General_trends"
#' )
#' }
run_temporal_model_predictions <- function(
  data_source,
  config_dir,
  model_dir,
  prediction_dir,
  rewrite = FALSE,
  max_prediction_draws = 1000L,
  prediction_range = "group_observed",
  verbose = TRUE
) {
  assertthat::assert_that(
    is.data.frame(data_source),
    dir.exists(config_dir),
    dir.exists(model_dir),
    is.logical(rewrite),
    length(rewrite) == 1L,
    msg = "Temporal-model prediction inputs are invalid."
  )

  dir.create(
    path = prediction_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  data_config <-
    RUtilpol::get_latest_file(
      file_name = "general_model_config_table",
      dir = config_dir,
      verbose = FALSE
    )

  data_pending <-
    data_config |>
    dplyr::filter(
      .data[["is_model_eligible"]],
      .data[["need_to_run"]] | .data[["need_to_be_evaluated"]]
    )

  assertthat::assert_that(
    nrow(data_pending) == 0L,
    msg = "Eligible temporal models must pass evaluation before prediction."
  )

  vec_model_ids <-
    data_config |>
    dplyr::filter(.data[["is_model_eligible"]]) |>
    dplyr::pull(.data[["model_id"]])

  list_predictions <-
    purrr::map(
      .x = vec_model_ids,
      .f = ~ predict_configured_temporal_model(
        model_id = .x,
        data_source = data_source,
        config_dir = config_dir,
        model_dir = model_dir,
        prediction_dir = prediction_dir,
        rewrite = rewrite,
        max_prediction_draws = max_prediction_draws,
        prediction_range = prediction_range,
        verbose = verbose
      )
    )

  res_predictions <-
    list_predictions |>
    purrr::compact() |>
    dplyr::bind_rows()

  return(res_predictions)
}
