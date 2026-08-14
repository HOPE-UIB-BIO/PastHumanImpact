#' @title Load a public target-store value
#' @description
#' Validate an upstream target-store contract and load one public target.
#' @param store Character scalar upstream target-store path.
#' @param target_name Character scalar public target name.
#' @param runner Character scalar prerequisite runner path.
#' @return The stored target value.
#' @examples
#' \dontrun{
#' data_inputs <- load_target_store_value(
#'   store = "Data/Targets_data/analyses_h1/inputs",
#'   target_name = "data_h1_inputs",
#'   runner = "R/analyses/02_h1_spatiotemporal_hvarpart/00_run.R"
#' )
#' }
load_target_store_value <- function(
  store,
  target_name,
  runner
) {
  validate_target_store_contract(
    store = store,
    target_names = target_name,
    runner = runner
  )

  res_value <-
    targets::tar_read_raw(
      name = target_name,
      store = store
    )

  return(res_value)
}
