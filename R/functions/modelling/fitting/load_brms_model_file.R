#' @title Load a referenced brms model file
#' @description
#' Load an exact configured model file, or locate the latest legacy file when
#' no explicit filename is available.
#' @param model_dir Existing directory containing fitted models.
#' @param model_file_name Optional exact model filename.
#' @param model_id Optional model identifier used only for legacy lookup.
#' @return Loaded model object.
#' @examples
#' \dontrun{
#' mod <- load_brms_model_file(
#'   model_dir = "Data/Temporal_models/Mods",
#'   model_file_name = "model_a__attempt__1.qs"
#' )
#' }
load_brms_model_file <- function(
  model_dir,
  model_file_name = NA_character_,
  model_id = NA_character_
) {
  assertthat::assert_that(
    is.character(model_dir),
    length(model_dir) == 1L,
    dir.exists(model_dir),
    msg = "`model_dir` must be an existing directory."
  )
  assertthat::assert_that(
    length(model_file_name) == 1L,
    is.character(model_id),
    length(model_id) == 1L,
    msg = "Model file identifiers must be character scalars."
  )
  assertthat::assert_that(
    is.na(model_file_name) || is.character(model_file_name),
    msg = "A non-missing `model_file_name` must be a character scalar."
  )

  model_file_name <-
    as.character(model_file_name)

  model_dir <-
    normalizePath(
      model_dir,
      winslash = "/",
      mustWork = TRUE
    )

  use_exact_file <-
    !is.na(model_file_name) && nzchar(model_file_name)

  if (
    isTRUE(use_exact_file)
  ) {
    path_model <-
      file.path(model_dir, model_file_name)

    assertthat::assert_that(
      file.exists(path_model),
      msg = "The configured model file does not exist."
    )

    res_model <-
      qs2::qs_read(path_model)

    return(res_model)
  }

  assertthat::assert_that(
    !is.na(model_id),
    nzchar(model_id),
    msg = "`model_id` is required for legacy model lookup."
  )

  res_model <-
    RUtilpol::get_latest_file(
      file_name = model_id,
      dir = model_dir,
      verbose = FALSE
    )

  return(res_model)
}
