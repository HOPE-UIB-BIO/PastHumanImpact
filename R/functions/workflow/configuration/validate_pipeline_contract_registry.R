#' @title Validate pipeline contract registry
#' @description
#' Validate unique pipeline ownership, stores, scripts, runners, and documented
#' public targets.
#' @param data_contracts
#' Pipeline contract registry with columns `pipeline_id`, `script`,
#' `store_relative_path`, `public_targets`, and `runner`, and optionally
#' `script_profile`.
#' @return Invisibly returns the validated registry.
#' @details
#' Pipeline identifiers and stores must be unique. A target script must be
#' owned by one row, unless every row that shares it declares a distinct,
#' non-missing `script_profile`; the shared script then resolves its profile
#' from the active store with `resolve_pipeline_profile()`. Rows whose script
#' is not shared may leave `script_profile` missing.
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
    !anyDuplicated(data_contracts[["store_relative_path"]]),
    all(!is.na(data_contracts[["public_targets"]])),
    all(nzchar(data_contracts[["public_targets"]])),
    all(file.exists(data_contracts[["script"]])),
    all(file.exists(data_contracts[["runner"]])),
    msg = "Pipeline contract registry is invalid."
  )

  vec_profiles <-
    if (
      "script_profile" %in% names(data_contracts)
    ) {
      as.character(data_contracts[["script_profile"]])
    } else {
      rep(NA_character_, nrow(data_contracts))
    }

  data_shared_scripts <-
    tibble::tibble(
      script = data_contracts[["script"]],
      script_profile = vec_profiles
    ) |>
    dplyr::group_by(.data[["script"]]) |>
    dplyr::filter(dplyr::n() > 1L) |>
    dplyr::summarise(
      profiles_declared = all(
        !is.na(.data[["script_profile"]]) &
          nzchar(.data[["script_profile"]])
      ),
      profiles_distinct = !anyDuplicated(.data[["script_profile"]]),
      .groups = "drop"
    )

  assertthat::assert_that(
    all(data_shared_scripts[["profiles_declared"]]),
    all(data_shared_scripts[["profiles_distinct"]]),
    msg = stringr::str_c(
      "Pipeline contract registry is invalid: rows sharing a script must ",
      "declare distinct non-missing `script_profile` values."
    )
  )

  return(invisible(data_contracts))
}
