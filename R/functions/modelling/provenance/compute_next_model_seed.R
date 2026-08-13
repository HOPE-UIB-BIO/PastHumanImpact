#' @title Compute configured model sampling seeds
#' @description
#' Increment seed attempts and deterministically generate replacement sampling
#' seeds for selected model configuration rows.
#' @param data_config Model configuration data frame.
#' @param model_ids Character vector of model IDs to update.
#' @param reason Character scalar describing why the seed changed.
#' @return Updated model configuration data frame.
#' @examples
#' dontrun{
#' config <- compute_next_model_seed(
#'   data_config = config,
#'   model_ids = "model_a",
#'   reason = "manual_rerun"
#' )
#' }
compute_next_model_seed <- function(
  data_config,
  model_ids,
  reason
) {
  assertthat::assert_that(
    is.data.frame(data_config),
    msg = "`data_config` must be a data frame."
  )

  required_columns <-
    c(
      "model_id",
      "seed_base",
      "seed_attempt",
      "sampling_seed",
      "seed_change_reason"
    )

  assertthat::assert_that(
    all(required_columns %in% names(data_config)),
    msg = "`data_config` is missing required seed columns."
  )
  assertthat::assert_that(
    is.character(model_ids),
    length(model_ids) > 0L,
    all(!is.na(model_ids)),
    all(nzchar(model_ids)),
    msg = "`model_ids` must contain non-empty character values."
  )
  assertthat::assert_that(
    is.character(reason),
    length(reason) == 1L,
    !is.na(reason),
    nzchar(reason),
    msg = "`reason` must be a non-empty character scalar."
  )
  assertthat::assert_that(
    all(model_ids %in% data_config[["model_id"]]),
    msg = "Unknown model IDs cannot have their seeds advanced."
  )

  rows_to_update <-
    data_config[["model_id"]] %in% model_ids

  res_config <- data_config
  res_config[["seed_attempt"]][rows_to_update] <-
    as.integer(res_config[["seed_attempt"]][rows_to_update]) + 1L
  res_config[["sampling_seed"]][rows_to_update] <-
    resolve_model_seed(
      model_id = res_config[["model_id"]][rows_to_update],
      seed_attempt = res_config[["seed_attempt"]][rows_to_update],
      seed_base = res_config[["seed_base"]][rows_to_update]
    )
  res_config[["seed_change_reason"]][rows_to_update] <- reason

  return(res_config)
}
