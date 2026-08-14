#' @title Compute an upstream target-store fingerprint
#' @description
#' Build a deterministic fingerprint from the recorded data hashes of public
#' targets in an upstream store.
#' @param store Character scalar upstream target-store path.
#' @param target_names Character vector of public target names.
#' @param runner Character scalar prerequisite runner path.
#' @return Character scalar fingerprint.
#' @examples
#' \dontrun{
#' fingerprint <- compute_target_store_fingerprint(
#'   store = "Data/Targets_data/analyses_h1/inputs",
#'   target_names = "data_h1_inputs",
#'   runner = "R/analyses/02_h1_spatiotemporal_hvarpart/00_run.R"
#' )
#' }
compute_target_store_fingerprint <- function(
  store,
  target_names,
  runner
) {
  data_meta <-
    validate_target_store_contract(
      store = store,
      target_names = target_names,
      runner = runner
    )

  res_fingerprint <-
    data_meta |>
    dplyr::arrange(.data[["name"]]) |>
    dplyr::transmute(
      value = stringr::str_c(.data[["name"]], .data[["data"]], sep = "=")
    ) |>
    dplyr::pull(.data[["value"]]) |>
    stringr::str_c(collapse = "|")

  return(res_fingerprint)
}
