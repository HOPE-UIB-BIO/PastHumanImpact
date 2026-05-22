# Function to flag the model to be rerun
#' @title Flag one model configuration to rerun
#' @description
#' Update one selected model row in the predictor-model config table and persist
#' the updated table to storage.
#' @param data_source Data frame with model config rows.
#' @param sel_region Character scalar region to match.
#' @param sel_climatezone Character scalar climatezone to match.
#' @param sel_variable Character scalar variable to match.
#' @return Invisible updated data frame.
flag_model_to_rerun <- function(
  data_source,
  sel_region, sel_climatezone, sel_variable
) {
  assertthat::assert_that(
    is.data.frame(data_source),
    msg = "`data_source` must be a data frame."
  )

  assertthat::assert_that(
    all(
      c(
        "region",
        "climatezone",
        "variable",
        "need_to_be_evaluated",
        "need_to_run",
        "last_evaluation_date"
      ) %in% names(data_source)
    ),
    msg = "`data_source` must contain required model config columns."
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

  assertthat::assert_that(
    exists("data_storage_path"),
    msg = "`data_storage_path` must be available in the session."
  )

  models_to_run_updated <-
    data_source %>%
    dplyr::mutate(
      need_to_be_evaluated = dplyr::case_when(
        .default = need_to_be_evaluated,
        region == sel_region &
          climatezone == sel_climatezone &
          variable == sel_variable ~ FALSE
      ),
      need_to_run = dplyr::case_when(
        .default = need_to_run,
        region == sel_region &
          climatezone == sel_climatezone &
          variable == sel_variable ~ TRUE
      ),
      last_evaluation_date = dplyr::case_when(
        .default = as.character(last_evaluation_date),
        region == sel_region &
          climatezone == sel_climatezone &
          variable == sel_variable ~ as.character(Sys.Date())
      )
    )

  RUtilpol::save_latest_file(
    object_to_save = models_to_run_updated,
    file_name = "predictor_models_config_table",
    dir = paste0(
      data_storage_path,
      "Predictor_models/"
    ),
    prefered_format = "csv",
    verbose = FALSE
  )

  return(invisible(models_to_run_updated))
}
