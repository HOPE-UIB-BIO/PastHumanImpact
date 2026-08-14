#' @title Evaluate model provenance audit results
#' @description
#' Add or update model-file, chain-seed, and provenance fields in a model
#' configuration table from one-row-per-model audit results.
#' @param data_config Model configuration data frame.
#' @param data_audit Model provenance audit data frame.
#' @return Updated model configuration data frame.
#' @examples
#' \dontrun{
#' config <- evaluate_model_provenance(config, audit)
#' }
evaluate_model_provenance <- function(
  data_config,
  data_audit
) {
  assertthat::assert_that(
    is.data.frame(data_config),
    is.data.frame(data_audit),
    msg = "Audit inputs must be data frames."
  )

  audit_columns <-
    c(
      "model_id",
      "model_file_name",
      "model_chain_seeds_json",
      "model_seed_source",
      "model_provenance_status",
      "model_audit_reason"
    )

  assertthat::assert_that(
    "model_id" %in% names(data_config),
    all(audit_columns %in% names(data_audit)),
    !anyDuplicated(data_config[["model_id"]]),
    !anyDuplicated(data_audit[["model_id"]]),
    all(data_audit[["model_id"]] %in% data_config[["model_id"]]),
    msg = "Audit inputs must contain unique, matching model IDs and fields."
  )

  res_config <- data_config

  missing_config_columns <-
    setdiff(audit_columns[-1], names(res_config))

  for (
    missing_column in missing_config_columns
  ) {
    res_config[[missing_column]] <- NA_character_
  }

  config_rows <-
    match(data_audit[["model_id"]], res_config[["model_id"]])

  for (
    audit_column in audit_columns[-1]
  ) {
    res_config[[audit_column]][config_rows] <-
      data_audit[[audit_column]]
  }

  return(res_config)
}
