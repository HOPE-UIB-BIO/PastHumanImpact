#' @title Create directory path recursively
#' @description Create a directory path recursively and silence base warnings.
#' @param dir_path Character scalar path to create.
#' @return Logical result from `dir.create()`.
make_dir <- function(dir_path) {
  assertthat::assert_that(
    is.character(dir_path) && length(dir_path) == 1,
    msg = "`dir_path` must be a single character path."
  )

  suppressWarnings(
    try(
      dir.create(
        dir_path,
        recursive = TRUE
      ),
      silent = TRUE
    )
  )
}