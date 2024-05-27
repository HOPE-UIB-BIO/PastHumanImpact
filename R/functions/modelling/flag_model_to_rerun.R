# Function to flag the model to be rerun
flag_model_to_rerun <- function(
    data_source,
    sel_region, sel_climatezone, sel_variable) {
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
      "Data/Predictor_models/"
    ),
    prefered_format = "csv",
    verbose = FALSE
  )
}
