#' @title Validate pipeline contract registry
#' @description
#' Validate unique pipeline ownership, stores, scripts, runners, and documented
#' public targets.
#' @param data_contracts Pipeline contract registry.
#' @return Invisibly returns the validated registry.
#' @examples
#' \dontrun{
#' validate_pipeline_contract_registry(contracts)
#' }
validate_pipeline_contract_registry <- function(data_contracts) {
  required_columns <-
    c(
      "pipeline_id",
      "script",
      "store_relative_path",
      "public_targets",
      "runner"
    )

  assertthat::assert_that(
    is.data.frame(data_contracts),
    all(required_columns %in% names(data_contracts)),
    nrow(data_contracts) > 0L,
    !anyDuplicated(data_contracts[["pipeline_id"]]),
    !anyDuplicated(data_contracts[["script"]]),
    !anyDuplicated(data_contracts[["store_relative_path"]]),
    all(!is.na(data_contracts[["public_targets"]])),
    all(nzchar(data_contracts[["public_targets"]])),
    all(file.exists(data_contracts[["script"]])),
    all(file.exists(data_contracts[["runner"]])),
    msg = "Pipeline contract registry is invalid."
  )

  return(invisible(data_contracts))
}
