#' @title Resolve the latest versioned data file
#' @description
#' Find the latest file matching a project data name and return its complete
#' path for use by `targets` file-format targets.
#' @param file_name Base project data name passed to `RUtilpol`.
#' @param dir Directory containing the versioned data files.
#' @return A character scalar containing the existing full file path.
#' @examples
#' \dontrun{
#' resolve_latest_file_path("data_meta", "D:/HumanImpact/Data/Meta")
#' }
resolve_latest_file_path <- function(file_name, dir) {
  assertthat::assert_that(
    is.character(file_name),
    length(file_name) == 1L,
    !is.na(file_name),
    is.character(dir),
    length(dir) == 1L,
    !is.na(dir),
    dir.exists(dir),
    msg = "Latest-file path inputs do not satisfy the contract."
  )

  latest_file_name <-
    RUtilpol::get_latest_file_name(
      file_name = file_name,
      dir = dir
    )

  if (
    length(latest_file_name) != 1L ||
      is.na(latest_file_name)
  ) {
    cli::cli_abort(
      "No versioned file matching {.val {file_name}} exists in {.path {dir}}."
    )
  }

  latest_file_path <-
    if (
      is_absolute_project_path(latest_file_name)
    ) {
      latest_file_name
    } else {
      file.path(dir, latest_file_name)
    }

  if (
    !file.exists(latest_file_path)
  ) {
    cli::cli_abort(
      "The resolved latest data file does not exist: {.path {latest_file_path}}"
    )
  }

  return(normalizePath(latest_file_path, winslash = "/"))
}
