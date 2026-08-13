#' @title Function to create a dataframe of m2 and time
#' @param data The vector of time m2
#' @return Data frame with columns `time` and `delta_m2`.

prepare_m2_time_data <- function(data) {
  assertthat::assert_that(
    is.atomic(data),
    msg = "`data` must be an atomic vector."
  )

  df <- data %>%
    data.frame(delta_m2 = .) %>%
    tibble::rownames_to_column("time")
  return(df)
}
