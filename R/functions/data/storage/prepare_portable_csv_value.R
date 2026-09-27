#' @title Prepare one list-column value for CSV export
#' @description Serialise a nested data frame as JSON or collapse another
#' vector-like value to a deterministic semicolon-delimited string.
#' @param value One list-column element.
#' @return A length-one character value.
#' @examples
#' prepare_portable_csv_value(c("a", "b"))
prepare_portable_csv_value <- function(value) {
  assertthat::assert_that(
    !is.environment(value),
    !is.function(value),
    msg = "Portable CSV values cannot be environments or functions."
  )
  if (is.data.frame(value)) {
    res <- jsonlite::toJSON(
      value,
      dataframe = "rows",
      auto_unbox = TRUE
    )
  } else {
    res <- stringr::str_c(unlist(value, use.names = FALSE), collapse = ";")
  }

  return(res)
}
