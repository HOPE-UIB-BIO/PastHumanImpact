#' @title Predict one configured temporal model
#' @description
#' Load one exact fitted model, create its dataset-age prediction grid, and
#' save either equal-weighted general trends or dataset-specific trajectories.
#' @param model_id Character scalar model identifier.
#' @param data_source Data frame containing all temporal model input data.
#' @param config_dir Existing directory containing the model configuration.
#' @param model_dir Existing directory containing fitted model files.
#' @param prediction_dir Existing directory for per-model prediction files.
#' @param rewrite Logical. If `TRUE`, replace an existing current prediction.
#' @param max_prediction_draws Maximum posterior draws used for prediction.
#' @param prediction_range Character scalar describing the prediction age
#' range. See `prepare_model_prediction_data()`.
#' @param prediction_estimand Character scalar selecting equal-weighted general
#' trends or dataset-specific trajectories. Only general trends update the
#' configuration prediction lifecycle state.
#' @param config_file_name Character scalar configuration basename.
#' @param verbose Logical. If `TRUE`, print save progress.
#' @return Prediction data frame, or `NULL` for ineligible or pending models.
#' @examples
#' \dontrun{
#' data_prediction <- predict_configured_temporal_model(
#'   model_id = "pap_temporal__n0__Europe__Temperate",
#'   data_source = data_general_model,
#'   config_dir = "Data/Temporal_models",
#'   model_dir = "Data/Temporal_models/Mods",
#'   prediction_dir = "Data/Temporal_models/General_trends"
#' )
#' }
predict_configured_temporal_model <- function(
  model_id,
  data_source,
  config_dir,
  model_dir,
  prediction_dir,
  rewrite = FALSE,
  max_prediction_draws = 1000L,
  prediction_range = c("configured", "group_observed"),
  prediction_estimand = c(
    "equal_weighted_dataset_mean",
    "dataset_specific"
  ),
  config_file_name = "general_model_config_table",
  verbose = TRUE
) {
  assertthat::assert_that(
    is.character(model_id),
    length(model_id) == 1L,
    !is.na(model_id),
    nzchar(model_id),
    msg = "`model_id` must be a non-empty character scalar."
  )
  assertthat::assert_that(
    is.data.frame(data_source),
    msg = "`data_source` must be a data frame."
  )
  assertthat::assert_that(
    is.character(config_dir),
    length(config_dir) == 1L,
    dir.exists(config_dir),
    is.character(model_dir),
    length(model_dir) == 1L,
    dir.exists(model_dir),
    is.character(prediction_dir),
    length(prediction_dir) == 1L,
    dir.exists(prediction_dir),
    msg = "Prediction lifecycle directories must exist."
  )
  assertthat::assert_that(
    is.logical(rewrite),
    length(rewrite) == 1L,
    !is.na(rewrite),
    assertthat::is.count(max_prediction_draws),
    is.logical(verbose),
    length(verbose) == 1L,
    !is.na(verbose),
    msg = "Prediction controls must be valid scalar values."
  )
  prediction_range <-
    match.arg(prediction_range)
  prediction_estimand <-
    match.arg(prediction_estimand)
  assertthat::assert_that(
    is.character(config_file_name),
    length(config_file_name) == 1L,
    !is.na(config_file_name),
    nzchar(config_file_name),
    msg = "`config_file_name` must be a non-empty character scalar."
  )

  config_dir <-
    normalizePath(config_dir, winslash = "/", mustWork = TRUE)
  model_dir <-
    normalizePath(model_dir, winslash = "/", mustWork = TRUE)
  prediction_dir <-
    normalizePath(prediction_dir, winslash = "/", mustWork = TRUE)

  data_config <-
    RUtilpol::get_latest_file(
      file_name = config_file_name,
      dir = config_dir,
      verbose = FALSE
    )
  model_config_row <-
    data_config %>%
    dplyr::filter(model_id == .env$model_id)

  assertthat::assert_that(
    nrow(model_config_row) == 1L,
    msg = "`model_id` must identify exactly one model configuration row."
  )

  if (
    isFALSE(model_config_row[["is_model_eligible"]][1]) ||
      isTRUE(model_config_row[["need_to_run"]][1]) ||
      isTRUE(model_config_row[["need_to_be_evaluated"]][1])
  ) {
    return(NULL)
  }

  prediction_file_exists <-
    list.files(prediction_dir) %>%
    startsWith(
      prefix = stringr::str_c(model_id, "_")
    ) %>%
    any()

  lifecycle_prediction_exists <-
    prediction_estimand == "dataset_specific" ||
    isTRUE(model_config_row[["prediction_written"]][1])

  if (
    isTRUE(prediction_file_exists) &&
      isFALSE(rewrite) &&
      isTRUE(lifecycle_prediction_exists)
  ) {
    res_existing <-
      RUtilpol::get_latest_file(
        file_name = model_id,
        dir = prediction_dir,
        verbose = FALSE
      )

    required_provenance_columns <-
      c(
        "source_model_file",
        "prediction_estimand",
        "prediction_range"
      )
    has_current_provenance <-
      all(required_provenance_columns %in% names(res_existing)) &&
      all(
        res_existing[["source_model_file"]] ==
          model_config_row[["model_file_name"]][1]
      ) &&
      all(
        res_existing[["prediction_estimand"]] ==
          prediction_estimand
      ) &&
      all(
        res_existing[["prediction_range"]] == prediction_range
      )

    if (
      isTRUE(has_current_provenance)
    ) {
      res_existing <-
        res_existing %>%
        dplyr::mutate(
          dplyr::across(
            dplyr::any_of("dataset_id"),
            as.character
          )
        )

      return(res_existing)
    }

    if (
      isTRUE(verbose)
    ) {
      cli::cli_inform(
        "Regenerating stale predictions for {.val {model_id}}."
      )
    }
  }

  mod <-
    load_brms_model_file(
      model_dir = model_dir,
      model_file_name = model_config_row[["model_file_name"]][1],
      model_id = model_id
    )
  data_new <-
    prepare_model_prediction_data(
      data_source = data_source,
      model_config_row = model_config_row,
      prediction_range = prediction_range
    )
  res_prediction <-
    predict_brms_model(
      mod = mod,
      newdata = data_new,
      model_config_row = model_config_row,
      max_prediction_draws = max_prediction_draws,
      prediction_range = prediction_range,
      prediction_estimand = prediction_estimand
    ) %>%
    dplyr::mutate(
      value = estimate,
      dplyr::across(
        dplyr::any_of("dataset_id"),
        as.character
      ),
      dplyr::across(
        dplyr::where(is.numeric),
        ~ round(.x, digits = 8)
      ),
      dplyr::across(
        dplyr::any_of(
          c("n_datasets_marginalised", "prediction_draws_used")
        ),
        as.integer
      )
    )

  RUtilpol::save_latest_file(
    object_to_save = res_prediction,
    file_name = model_id,
    dir = prediction_dir,
    prefered_format = "csv",
    verbose = verbose
  )

  if (
    prediction_estimand == "equal_weighted_dataset_mean"
  ) {
    data_config_updated <-
      RUtilpol::get_latest_file(
        file_name = config_file_name,
        dir = config_dir,
        verbose = FALSE
      ) %>%
      dplyr::mutate(
        prediction_written = dplyr::case_when(
          .default = prediction_written,
          model_id == .env$model_id ~ TRUE
        ),
        last_prediction_date = dplyr::case_when(
          .default = as.character(last_prediction_date),
          model_id == .env$model_id ~ as.character(Sys.Date())
        )
      )

    RUtilpol::save_latest_file(
      object_to_save = data_config_updated,
      file_name = config_file_name,
      dir = config_dir,
      prefered_format = "csv",
      verbose = verbose
    )
  }

  return(res_prediction)
}
