#' @title Resolve a pipeline target store
#' @description
#' Resolve one stable target-store path below the project data root.
#' @param data_storage_path Character scalar project data root.
#' @param store_relative_path Character scalar path below `Targets_data`.
#' @return Character scalar target-store path.
#' @examples
#' \dontrun{
#' path_store <- resolve_pipeline_store_path(
#'   data_storage_path = "D:/HumanImpact/Data",
#'   store_relative_path = "analyses_h1/inputs"
#' )
#' }
resolve_pipeline_store_path <- function(
  data_storage_path,
  store_relative_path
) {
  assertthat::assert_that(
    is.character(data_storage_path),
    length(data_storage_path) == 1L,
    is.character(store_relative_path),
    length(store_relative_path) == 1L,
    !is.na(store_relative_path),
    nzchar(store_relative_path),
    msg = "Pipeline store paths must be non-empty character scalars."
  )

  assertthat::assert_that(
    !grepl("(^|[\\\\/])\\.\\.([\\\\/]|$)", store_relative_path),
    msg = "Pipeline store paths may not traverse above `Targets_data`."
  )

  res_path <-
    file.path(
      data_storage_path,
      "Targets_data",
      store_relative_path
    )

  return(res_path)
}
