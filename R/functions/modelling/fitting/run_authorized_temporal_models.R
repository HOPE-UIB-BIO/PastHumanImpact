#' @title Run authorized temporal models
#' @description
#' Run only temporal-model fitting attempts that passed the explicit two-key
#' authorization gate.
#' @param data_authorized Authorized request rows containing model and request
#' identifiers.
#' @param data_source Temporal model input data.
#' @param config_dir Existing temporal-model configuration directory.
#' @param model_dir Existing immutable fitted-model directory.
#' @param path_history Character scalar run-history path.
#' @param git_commit Optional Git commit identifier.
#' @param git_is_dirty Optional logical worktree state.
#' @param verbose Logical scalar controlling progress messages.
#' @return Tibble with one row per attempted authorization, or an empty tibble.
#' @examples
#' \dontrun{
#' runs <- run_authorized_temporal_models(
#'   data_authorized = authorized,
#'   data_source = model_data,
#'   config_dir = "Data/Temporal_models",
#'   model_dir = "Data/Temporal_models/Mods",
#'   path_history = "Data/Temporal_models/run_history.csv"
#' )
#' }
run_authorized_temporal_models <- function(
  data_authorized,
  data_source,
  config_dir,
  model_dir,
  path_history,
  git_commit = NA_character_,
  git_is_dirty = NA,
  verbose = TRUE
) {
  assertthat::assert_that(
    is.data.frame(data_authorized),
    all(c("model_id", "request_id") %in% names(data_authorized)),
    is.data.frame(data_source),
    is.character(config_dir),
    length(config_dir) == 1L,
    dir.exists(config_dir),
    is.character(model_dir),
    length(model_dir) == 1L,
    dir.exists(model_dir),
    msg = "Authorized temporal-model fitting inputs are invalid."
  )

  if (
    nrow(data_authorized) == 0L
  ) {
    res <-
      tibble::tibble(
        model_id = character(),
        request_id = character(),
        attempted = logical()
      )

    return(res)
  }

  purrr::pwalk(
    .l = data_authorized |>
      dplyr::select(
        dplyr::all_of(c("model_id", "request_id"))
      ),
    .f = ~ run_configured_temporal_model(
      model_id = ..1,
      request_id = ..2,
      data_source = data_source,
      config_dir = config_dir,
      model_dir = model_dir,
      path_history = path_history,
      git_commit = git_commit,
      git_is_dirty = git_is_dirty,
      verbose = verbose
    )
  )

  res_runs <-
    data_authorized |>
    dplyr::transmute(
      model_id = .data[["model_id"]],
      request_id = .data[["request_id"]],
      attempted = TRUE
    )

  return(res_runs)
}
