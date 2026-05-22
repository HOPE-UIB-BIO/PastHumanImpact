#' @title Add predictor factor
#' @description Cast `predictor` to the canonical `human`/`climate` levels.
#' @param data_source Data frame with column `predictor`.
#' @return Data frame with factor column `predictor`.
add_predictor_as_factor <- function(data_source) {
  assertthat::assert_that(
    is.data.frame(data_source),
    msg = "`data_source` must be a data frame."
  )

  assertthat::assert_that(
    "predictor" %in% names(data_source),
    msg = "`data_source` must contain `predictor`."
  )

  res_data <-
    data_source %>%
    dplyr::mutate(
      predictor = factor(
        predictor,
        levels = c("human", "climate")
      )
    )

  return(res_data)
}
