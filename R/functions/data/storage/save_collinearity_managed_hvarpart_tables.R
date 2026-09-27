#' @title Save portable collinearity-managed HVarPart audit tables
#' @description Write named rectangular evidence tables to explicit CSV paths,
#' serialising list columns deterministically for a portable audit trail.
#' @param data_tables Named list of data frames.
#' @param file_paths Named character vector of output paths.
#' @return Character vector of written file paths.
#' @examples
#' \dontrun{save_collinearity_managed_hvarpart_tables(list(a = data.frame(x = 1)), c(a = "a.csv"))}
save_collinearity_managed_hvarpart_tables <- function(data_tables, file_paths) {
  assertthat::assert_that(
    is.list(data_tables), !is.null(names(data_tables)),
    is.character(file_paths), !is.null(names(file_paths)),
    setequal(names(data_tables), names(file_paths)),
    all(purrr::map_lgl(data_tables, is.data.frame)),
    msg = "Collinearity-managed evidence table inputs do not satisfy the contract."
  )
  purrr::iwalk(data_tables, .f = ~ {
    data <- .x
    table_name <- .y
    path <- file_paths[[table_name]]
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    portable <- data |>
      dplyr::mutate(
        dplyr::across(
          where(is.list),
          ~ purrr::map_chr(.x, prepare_portable_csv_value)
        )
      )
    readr::write_csv(portable, path, na = "")
  })
  res <- unname(file_paths)

  return(res)
}
