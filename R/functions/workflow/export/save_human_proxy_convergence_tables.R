#' @title Save human-proxy convergence evidence tables
#' @description
#' Write matched values, bin summaries, correlations, and provenance to named
#' CSV or compressed CSV paths.
#' @param data_tables Named list of data frames.
#' @param file_paths Named character vector of output paths.
#' @return Named normalized output paths.
#' @examples
#' \dontrun{
#' save_human_proxy_convergence_tables(list(values = x), c(values = "x.csv"))
#' }
save_human_proxy_convergence_tables <- function(data_tables, file_paths) {
  assertthat::assert_that(
    is.list(data_tables),
    length(data_tables) > 0L,
    all(purrr::map_lgl(data_tables, is.data.frame)),
    is.character(file_paths),
    length(file_paths) == length(data_tables),
    !is.null(names(data_tables)),
    !is.null(names(file_paths)),
    identical(names(data_tables), names(file_paths)),
    msg = "Human-proxy table export inputs do not satisfy the contract."
  )

  purrr::walk(
    file_paths,
    ~ dir.create(
      dirname(.x),
      recursive = TRUE,
      showWarnings = FALSE
    )
  )

  purrr::walk2(data_tables, file_paths, readr::write_csv)

  res_paths <-
    normalizePath(file_paths, winslash = "/", mustWork = TRUE)

  names(res_paths) <- names(file_paths)

  return(res_paths)
}
