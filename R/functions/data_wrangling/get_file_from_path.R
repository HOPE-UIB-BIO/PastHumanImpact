#' @title Read An RDS File From Disk
#' @description
#' Read an R object stored as an `.rds` file from a caller-supplied path.
#' @param path Character scalar path to an existing `.rds` file.
#' @return
#' The R object stored in the file.
get_file_from_path <- function(path) {
  assertthat::assert_that(
    is.character(path),
    length(path) == 1,
    msg = "`path` must be a single character value."
  )
  assertthat::assert_that(
    file.exists(path),
    msg = "`path` must point to an existing file."
  )

  readr::read_rds(path) %>%
    return()
}
