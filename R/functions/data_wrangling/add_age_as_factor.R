#' @title Add age factor in kyr
#' @description Convert `age` from years to kyr and cast to an ordered factor.
#' @param data_source Data frame with numeric column `age`.
#' @return Data frame with `age` converted to a factor.
add_age_as_factor <- function(data_source) {
  assertthat::assert_that(
    is.data.frame(data_source),
    msg = "`data_source` must be a data frame."
  )

  assertthat::assert_that(
    "age" %in% names(data_source),
    msg = "`data_source` must contain `age`."
  )

  assertthat::assert_that(
    is.numeric(data_source$age),
    msg = "`data_source$age` must be numeric."
  )

  res_data <-
    data_source %>%
    dplyr::mutate(
      age = age / 1e3,
      age = factor(
        age,
        levels = seq(8.5, 0, -0.5)
      )
    )

  return(res_data)
}