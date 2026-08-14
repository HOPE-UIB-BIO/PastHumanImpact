#' @title Save reconciled temporal-model lifecycle state
#' @description
#' Publish reconciled lifecycle state without changing fitting authorization.
#' @param data_config Reconciled temporal-model configuration.
#' @param directory Existing temporal-model artifact directory.
#' @param current_date Date used by the versioned configuration filename.
#' @return Normalized path to the current lifecycle configuration file.
#' @examples
#' \dontrun{
#' path <- save_temporal_model_lifecycle(
#'   data_config = config,
#'   directory = "Data/Temporal_models"
#' )
#' }
save_temporal_model_lifecycle <- function(
  data_config,
  directory,
  current_date = Sys.Date()
) {
  assertthat::assert_that(
    is.data.frame(data_config),
    is.character(directory),
    length(directory) == 1L,
    dir.exists(directory),
    inherits(current_date, "Date"),
    length(current_date) == 1L,
    msg = "Temporal-model lifecycle save inputs are invalid."
  )

  normalized_directory <-
    normalizePath(
      directory,
      winslash = "/",
      mustWork = TRUE
    )

  RUtilpol::save_latest_file(
    object_to_save = data_config,
    file_name = "general_model_config_table",
    dir = normalized_directory,
    current_date = current_date,
    prefered_format = "csv",
    use_sha = FALSE,
    verbose = FALSE
  )

  current_name <-
    RUtilpol::get_latest_file_name(
      file_name = "general_model_config_table",
      dir = normalized_directory,
      verbose = FALSE
    )

  res_path <-
    file.path(normalized_directory, current_name)

  return(res_path)
}
