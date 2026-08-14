#' @title Add a temporal configuration reference
#' @description
#' Add the declared model-policy hash while preserving an existing non-missing
#' reference unless replacement is explicitly requested.
#' @param data_config Temporal-model configuration.
#' @param reference_hash Character scalar policy hash.
#' @param overwrite Logical scalar controlling replacement.
#' @return Configuration tibble with configuration_reference_hash.
#' @examples
#' add_temporal_configuration_reference(
#'   data_config = tibble::tibble(model_id = "a"),
#'   reference_hash = "policy"
#' )
add_temporal_configuration_reference <- function(
  data_config,
  reference_hash,
  overwrite = FALSE
) {
  assertthat::assert_that(
    is.data.frame(data_config),
    is.character(reference_hash),
    length(reference_hash) == 1L,
    !is.na(reference_hash),
    nzchar(reference_hash),
    is.logical(overwrite),
    length(overwrite) == 1L,
    !is.na(overwrite),
    msg = "Temporal configuration reference inputs are invalid."
  )

  if (
    !"configuration_reference_hash" %in% names(data_config)
  ) {
    data_config[["configuration_reference_hash"]] <- reference_hash
  }

  if (
    isTRUE(overwrite)
  ) {
    data_config[["configuration_reference_hash"]] <- reference_hash
  } else {
    missing_reference <-
      is.na(data_config[["configuration_reference_hash"]]) |
      !nzchar(data_config[["configuration_reference_hash"]])

    data_config[["configuration_reference_hash"]][missing_reference] <-
      reference_hash
  }

  return(data_config)
}
