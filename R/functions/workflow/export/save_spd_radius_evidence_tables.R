#' @title Write SPD-radius sensitivity evidence tables
#' @description Write named source tables to semantic CSV paths.
#' @param data_tables Named list of data frames.
#' @param file_paths Named character vector of output paths.
#' @return Normalized paths to written files.
#' @examples
#' \dontrun{
#' save_spd_radius_evidence_tables(list(a = table), c(a = "a.csv"))
#' }
save_spd_radius_evidence_tables <- function(
  data_tables,
  file_paths
) {
  assertthat::assert_that(
    is.list(data_tables),
    length(data_tables) > 0L,
    all(purrr::map_lgl(data_tables, is.data.frame)),
    is.character(file_paths),
    length(file_paths) == length(data_tables),
    !is.null(names(data_tables)),
    !is.null(names(file_paths)),
    identical(names(data_tables), names(file_paths)),
    msg = "SPD radius evidence export inputs do not satisfy the contract."
  )

  purrr::walk(file_paths, ~ dir.create(
    dirname(.x),
    recursive = TRUE,
    showWarnings = FALSE
  ))
  purrr::walk2(data_tables, file_paths, readr::write_csv)

  res <-
    normalizePath(file_paths, winslash = "/", mustWork = TRUE)
  names(res) <- names(file_paths)

  return(res)
}
