#' @title Run pending temporal-model evaluations
#' @description
#' Evaluate only eligible temporal models explicitly marked as awaiting
#' evaluation in the lifecycle configuration.
#' @param config_dir Existing temporal-model configuration directory.
#' @param model_dir Existing fitted-model directory.
#' @param path_history Character scalar run-history path.
#' @param git_commit Optional Git commit identifier.
#' @param git_is_dirty Optional logical worktree state.
#' @param verbose Logical scalar controlling progress messages.
#' @return Tibble listing the model IDs selected for evaluation.
#' @examples
#' \dontrun{
#' evaluated <- run_pending_temporal_model_evaluations(
#'   config_dir = "Data/Temporal_models",
#'   model_dir = "Data/Temporal_models/Mods",
#'   path_history = "Data/Temporal_models/run_history.csv"
#' )
#' }
run_pending_temporal_model_evaluations <- function(
  config_dir,
  model_dir,
  path_history,
  git_commit = NA_character_,
  git_is_dirty = NA,
  verbose = TRUE
) {
  assertthat::assert_that(
    is.character(config_dir),
    length(config_dir) == 1L,
    dir.exists(config_dir),
    is.character(model_dir),
    length(model_dir) == 1L,
    dir.exists(model_dir),
    msg = "Temporal-model evaluation directories must exist."
  )

  data_config <-
    RUtilpol::get_latest_file(
      file_name = "general_model_config_table",
      dir = config_dir,
      verbose = FALSE
    )

  vec_model_ids <-
    data_config |>
    dplyr::filter(
      .data[["is_model_eligible"]],
      .data[["need_to_be_evaluated"]]
    ) |>
    dplyr::pull(.data[["model_id"]])

  purrr::walk(
    .x = vec_model_ids,
    .f = ~ evaluate_configured_temporal_model(
      model_id = .x,
      config_dir = config_dir,
      model_dir = model_dir,
      path_history = path_history,
      pareto_k_threshold = 0.7,
      loo_threshold = 0.1,
      rhat_threshold = 1.1,
      rhat_threshold_quantile = 0.9,
      git_commit = git_commit,
      git_is_dirty = git_is_dirty,
      verbose = verbose
    )
  )

  res_evaluations <-
    tibble::tibble(model_id = vec_model_ids)

  return(res_evaluations)
}
