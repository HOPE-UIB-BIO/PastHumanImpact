#' @title Resolve one model configuration to rerun
#' @description
#' Update one selected model row in the temporal model config table and persist
#' the updated table to storage.
#' @param data_source Data frame with model config rows.
#' @param sel_region Character scalar region to match.
#' @param sel_climatezone Character scalar climatezone to match.
#' @param sel_variable Character scalar variable to match.
#' @param sel_model_id Optional character scalar model ID for general model
#' config tables.
#' @param config_file_name Character scalar saved config table basename.
#' @param storage_subdir Character scalar subdirectory under `data_storage_path`.
#' @param save_table Logical. If `TRUE`, persist the updated table.
#' @param advance_seed Logical. If `TRUE`, advance the selected model seed.
#' @param seed_change_reason Character reason recorded for the seed change.
#' @return Invisible updated data frame.
resolve_model_rerun_flag <- function(
  data_source,
  sel_region = NULL,
  sel_climatezone = NULL,
  sel_variable = NULL,
  sel_model_id = NULL,
  config_file_name = "general_model_config_table",
  storage_subdir = "Temporal_models",
  save_table = TRUE,
  advance_seed = TRUE,
  seed_change_reason = "manual_rerun"
) {
  assertthat::assert_that(
    is.data.frame(data_source),
    msg = "`data_source` must be a data frame."
  )

  assertthat::assert_that(
    all(
      c(
        "need_to_be_evaluated",
        "need_to_run",
        "last_evaluation_date"
      ) %in% names(data_source)
    ),
    msg = "`data_source` must contain required model config columns."
  )
  assertthat::assert_that(
    is.logical(advance_seed),
    length(advance_seed) == 1L,
    !is.na(advance_seed),
    msg = "`advance_seed` must be one non-missing logical value."
  )

  if (
    isTRUE(advance_seed)
  ) {
    seed_columns <-
      c(
        "model_id",
        "seed_base",
        "seed_attempt",
        "sampling_seed",
        "seed_change_reason"
      )

    assertthat::assert_that(
      all(seed_columns %in% names(data_source)),
      msg = "Seed advancement requires model seed configuration columns."
    )
  }

  use_model_id <-
    isFALSE(is.null(sel_model_id))

  if (
    isTRUE(use_model_id)
  ) {
    assertthat::assert_that(
      "model_id" %in% names(data_source),
      msg = "`data_source` must contain `model_id` when using `sel_model_id`."
    )
    assertthat::assert_that(
      is.character(sel_model_id) && length(sel_model_id) == 1,
      msg = "`sel_model_id` must be a single character value."
    )
  } else {
    assertthat::assert_that(
      all(c("region", "climatezone", "variable") %in% names(data_source)),
      msg = paste(
        "`data_source` must contain `region`, `climatezone`, and `variable`",
        "when not using `sel_model_id`."
      )
    )
    assertthat::assert_that(
      is.character(sel_region) && length(sel_region) == 1,
      msg = "`sel_region` must be a single character value."
    )
    assertthat::assert_that(
      is.character(sel_climatezone) && length(sel_climatezone) == 1,
      msg = "`sel_climatezone` must be a single character value."
    )
    assertthat::assert_that(
      is.character(sel_variable) && length(sel_variable) == 1,
      msg = "`sel_variable` must be a single character value."
    )
  }

  rows_to_update <-
    if (
      isTRUE(use_model_id)
    ) {
      data_source[["model_id"]] == sel_model_id
    } else {
      data_source[["region"]] == sel_region &
        data_source[["climatezone"]] == sel_climatezone &
        data_source[["variable"]] == sel_variable
    }

  models_to_run_updated <-
    data_source %>%
    dplyr::mutate(
      need_to_be_evaluated = dplyr::case_when(
        .default = need_to_be_evaluated,
        rows_to_update ~ FALSE
      ),
      need_to_run = dplyr::case_when(
        .default = need_to_run,
        rows_to_update ~ TRUE
      ),
      last_evaluation_date = dplyr::case_when(
        .default = as.character(last_evaluation_date),
        rows_to_update ~ as.character(Sys.Date())
      )
    )

  if (
    isTRUE(advance_seed) && any(rows_to_update)
  ) {
    models_to_run_updated <-
      compute_next_model_seed(
        data_config = models_to_run_updated,
        model_ids = models_to_run_updated[["model_id"]][rows_to_update],
        reason = seed_change_reason
      )
  }

  if (
    isTRUE(save_table)
  ) {
    assertthat::assert_that(
      exists("data_storage_path"),
      msg = "`data_storage_path` must be available in the session."
    )

    RUtilpol::save_latest_file(
      object_to_save = models_to_run_updated,
      file_name = config_file_name,
      dir = file.path(data_storage_path, storage_subdir),
      prefered_format = "csv",
      verbose = FALSE
    )
  }

  return(invisible(models_to_run_updated))
}
