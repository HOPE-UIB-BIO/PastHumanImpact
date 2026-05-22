#' @title Transform time IDs to ages
#' @description
#' Transform CHELSA TraCE21k `time_id` values into age estimates comparable to
#' other project data using a reference translation table.
#' @param data_source
#' Data frame with at least `time_id`. If `time_month_value` is missing, it is
#' added as zero.
#' @param trans_data
#' Data frame with columns `timeID`, `startyear`, and `endyear` used to
#' transform `time_id` values.
#' @return
#' Data frame with joined translation columns and added `mid_age`, `age_greg`,
#' and `age`.
transform_ages <- function(
  data_source,
  trans_data
) {
  assertthat::assert_that(
    is.data.frame(data_source),
    msg = "`data_source` must be a data frame."
  )

  assertthat::assert_that(
    is.data.frame(trans_data),
    msg = "`trans_data` must be a data frame."
  )

  assertthat::assert_that(
    "time_id" %in% names(data_source),
    msg = "`data_source` must contain `time_id`."
  )

  assertthat::assert_that(
    all(c("timeID", "startyear", "endyear") %in% names(trans_data)),
    msg = "`trans_data` must contain `timeID`, `startyear`, and `endyear`."
  )

  if (!"time_month_value" %in% names(data_source)) {
    data_source <-
      data_source %>%
      dplyr::mutate(
        time_month_value = 0
      )
  }

  assertthat::assert_that(
    is.numeric(data_source$time_month_value),
    msg = "`time_month_value` must be numeric."
  )

  res_data <-
    data_source %>%
    dplyr::inner_join(trans_data, by = c("time_id" = "timeID")) %>%
    dplyr::mutate(
      mid_age = round((startyear + endyear) / 2, digits = 0),
      # convert from gregorian to before present (BP)
      age_greg = -(mid_age - 1950),
      age = age_greg + time_month_value
    )

  return(res_data)
}
