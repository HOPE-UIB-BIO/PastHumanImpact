#' @title Test whether a path is absolute
#' @description
#' Identify Windows drive, Windows UNC, and root-based absolute paths.
#' @param path_input Character scalar path to inspect.
#' @return A logical scalar indicating whether the path is absolute.
#' @examples
#' is_absolute_project_path("C:/project/file.R")
#' is_absolute_project_path("R/tests/testthat")
is_absolute_project_path <- function(path_input) {
  assertthat::assert_that(
    is.character(path_input),
    length(path_input) == 1L,
    !is.na(path_input),
    msg = "`path_input` must be one non-missing character path."
  )

  res_is_absolute <-
    grepl("^[A-Za-z]:[/\\\\]|^/|^\\\\\\\\", path_input)

  return(res_is_absolute)
}
