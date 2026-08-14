#' @title Validate a target-store contract
#' @description
#' Verify that an upstream target store exists and contains every required
#' complete public target.
#' @param store Character scalar upstream target-store path.
#' @param target_names Character vector of required public target names.
#' @param runner Character scalar command or script identifying the prerequisite
#' pipeline runner.
#' @return Invisibly returns target metadata for the required targets.
#' @examples
#' \dontrun{
#' validate_target_store_contract(
#'   store = "Data/Targets_data/analyses_h1/inputs",
#'   target_names = "data_h1_inputs",
#'   runner = "R/analyses/02_h1_spatiotemporal_hvarpart/00_run.R"
#' )
#' }
validate_target_store_contract <- function(
  store,
  target_names,
  runner
) {
  assertthat::assert_that(
    is.character(store),
    length(store) == 1L,
    is.character(target_names),
    length(target_names) > 0L,
    all(!is.na(target_names)),
    all(nzchar(target_names)),
    is.character(runner),
    length(runner) == 1L,
    !is.na(runner),
    nzchar(runner),
    msg = "Target-store contract arguments are invalid."
  )

  if (
    !dir.exists(store)
  ) {
    cli::cli_abort(
      c(
        "The required upstream target store does not exist:",
        "{.path {store}}",
        "Run {.path {runner}} first."
      )
    )
  }

  data_meta <-
    targets::tar_meta(
      fields = c("name", "data"),
      complete_only = TRUE,
      store = store
    ) |>
    dplyr::filter(.data[["name"]] %in% target_names)

  missing_targets <-
    setdiff(target_names, data_meta[["name"]])

  if (
    length(missing_targets) > 0L
  ) {
    cli::cli_abort(
      c(
        "The upstream store is missing public targets:",
        "{.val {missing_targets}}",
        "Run {.path {runner}} first."
      )
    )
  }

  return(invisible(data_meta))
}
