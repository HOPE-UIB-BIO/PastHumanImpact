#' @title Attach analytical-unit identifiers to diagnostics
#' @description Repeat a one-row identifier table and bind it to each row of a
#' diagnostic table while preserving an empty diagnostic unchanged.
#' @param data_diagnostic Diagnostic table.
#' @param data_identifiers One-row analytical-unit identifier table.
#' @return The diagnostic table with identifiers prepended.
#' @examples
#' prepare_hvarpart_diagnostic_identifiers(
#'   data.frame(value = 1:2), data.frame(dataset_id = "a")
#' )
prepare_hvarpart_diagnostic_identifiers <- function(
  data_diagnostic,
  data_identifiers
) {
  assertthat::assert_that(
    is.data.frame(data_diagnostic),
    is.data.frame(data_identifiers),
    nrow(data_identifiers) == 1L,
    msg = "HVarPart diagnostic identifiers do not satisfy the contract."
  )
  if (nrow(data_diagnostic) == 0L) return(data_diagnostic)

  res <- dplyr::bind_cols(
    data_identifiers[rep(1L, nrow(data_diagnostic)), , drop = FALSE],
    data_diagnostic
  )

  return(res)
}
