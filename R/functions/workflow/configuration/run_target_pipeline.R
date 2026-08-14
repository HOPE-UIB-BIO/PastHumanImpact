#' @title Run one target pipeline
#' @description
#' Run one target script in its dedicated external store and optionally open
#' its target-only dependency graph.
#' @param script Character scalar target pipeline script path.
#' @param store Character scalar target store path.
#' @param visualise Logical scalar controlling dependency-graph display.
#' @return Invisibly returns `NULL` after the pipeline completes.
#' @examples
#' \dontrun{
#' run_target_pipeline(
#'   script = "R/analyses/example/pipeline.R",
#'   store = "Data/Targets_data/example"
#' )
#' }
run_target_pipeline <- function(
  script,
  store,
  visualise = FALSE
) {
  assertthat::assert_that(
    is.character(script),
    length(script) == 1L,
    file.exists(script),
    is.character(store),
    length(store) == 1L,
    is.logical(visualise),
    length(visualise) == 1L,
    !is.na(visualise),
    msg = "Target pipeline arguments are invalid."
  )

  targets::tar_make(
    script = script,
    store = store
  )

  if (
    isTRUE(visualise)
  ) {
    targets::tar_visnetwork(
      targets_only = TRUE,
      script = script,
      store = store
    )
  }

  return(invisible(NULL))
}
