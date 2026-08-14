#' @title Split HVarPart time bins into single-group data frames
#' @description Create one list element per region-age row for target branching.
#' @param data_source Region-age data frame.
#' @return A list of one-row data frames.
#' @examples
#' \dontrun{
#' prepare_hvarpart_timebin_groups(time_bins)
#' }
prepare_hvarpart_timebin_groups <- function(data_source) {
  assertthat::assert_that(
    is.data.frame(data_source),
    all(c("region", "age", "data_merge") %in% names(data_source)),
    is.list(data_source[["data_merge"]]),
    msg = "Time-bin split inputs do not satisfy the contract."
  )

  res_groups <-
    base::split(
      x = data_source,
      f = seq_len(nrow(data_source))
    )

  return(res_groups)
}
