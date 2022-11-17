#' @title Test if levels are within the limit of age extrapolation
#' @param data_level Data.frame with ages of levels
#' @param data_chron_control_limits Vector of two (min and max age of control
#' points)
#' @param maximum_age_extrapolation number of the maximum allowed
#' extrapolation
#' @param test_quantiles Test quantiles of age prediction?
#' @return Vector with valid `sample_ids`
#' @description Select only levels, which has the age (our quantiles)
#'   younger than last chron.control point + `maximum_age_extrapolation` &
#'   older than first chron.control point - `maximum_age_extrapolation`
get_sampleid_extrapol <-
  function(data_level,
           data_chron_control_limits,
           maximum_age_extrapolation,
           test_quantiles = FALSE) {
    check_class("data_level", "data.frame")
    
    check_class("data_chron_control_limits", "numeric")
    
    assertthat::assert_that(
      length(data_chron_control_limits) == 2,
      msg = "'data_chron_control_limits' must be a vector of two (min, max)"
    )
    
    check_class("test_quantiles", "logical")
    
    if (
      test_quantiles == TRUE
    ) {
      check_col_names("data_level", c("upper", "lower"))
    } else {
      check_col_names("data_level", "age")
    }
    
    assertthat::assert_that(
      length(data_chron_control_limits) == 2,
      msg = "'data_chron_control_limits' has to have 2 values"
    )
    
    check_class("maximum_age_extrapolation", "numeric")
    
    if (
      test_quantiles == FALSE
    ) {
      data_level <-
        data_level %>%
        dplyr::mutate(
          lower = age,
          upper = age
        )
    }
    
    data_subset <-
      data_level %>%
      dplyr::filter(
        upper < data_chron_control_limits[2] + maximum_age_extrapolation &
          lower > data_chron_control_limits[1] - maximum_age_extrapolation
      )
    
    if (
      nrow(data_subset) > 0
    ) {
      return(data_subset$sample_id)
    } else {
      return(NA)
    }
  }