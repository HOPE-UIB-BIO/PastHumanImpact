#' @title Save an immutable brms model run
#' @description
#' Save one fitted model under its unique run ID without replacing any earlier
#' model attempt.
#' @param mod Fitted `brmsfit` object.
#' @param model_dir Existing directory for fitted model files.
#' @param run_id Character scalar unique run identifier.
#' @return Character scalar saved model filename.
#' @examples
#' \dontrun{
#' model_file_name <- save_brms_model_run(
#'   mod = mod,
#'   model_dir = "Data/Temporal_models/Mods",
#'   run_id = "model_a__attempt__1__seed__123"
#' )
#' }
save_brms_model_run <- function(
  mod,
  model_dir,
  run_id
) {
  assertthat::assert_that(
    inherits(mod, "brmsfit"),
    msg = "`mod` must be a fitted brms model."
  )
  assertthat::assert_that(
    is.character(model_dir),
    length(model_dir) == 1L,
    dir.exists(model_dir),
    msg = "`model_dir` must be an existing directory."
  )
  assertthat::assert_that(
    is.character(run_id),
    length(run_id) == 1L,
    !is.na(run_id),
    nzchar(run_id),
    grepl("^[[:alnum:]_.-]+$", run_id),
    msg = "`run_id` must be a filesystem-safe character scalar."
  )

  model_dir <-
    normalizePath(
      model_dir,
      winslash = "/",
      mustWork = TRUE
    )
  res_file_name <-
    stringr::str_c(run_id, ".qs")
  path_model <-
    file.path(model_dir, res_file_name)

  if (
    file.exists(path_model)
  ) {
    cli::cli_abort(
      c(
        "The model run file already exists and will not be replaced:",
        "{.path {path_model}}"
      )
    )
  }

  qs2::qs_save(
    object = mod,
    file = path_model
  )

  assertthat::assert_that(
    file.exists(path_model),
    msg = "The fitted model file was not written."
  )

  return(res_file_name)
}
