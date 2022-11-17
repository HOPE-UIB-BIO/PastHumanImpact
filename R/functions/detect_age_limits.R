#' @title Test if sequence is long enough for selected age criteria
#' @param data_source Data.frame with level ages
#' @param age_limit_young Young age limit for selected site
#' @param age_limit_old Old age limit for selected site
#' @param test_quantiles Test quantiles of age prediction?
#' @return Logical
#' @description Test if the youngest limit of any level is younger than
#' age limit as well as if the oldest limit of any level is older than age
#' limit
detect_age_limits <-
  function(data_source,
           age_limit_young,
           age_limit_old,
           test_quantiles = FALSE) {
    check_class("data_source", "data.frame")
    
    check_class("test_quantiles", "logical")
    
    if (
      test_quantiles == TRUE
    ) {
      check_col_names("data_source", c("upper", "lower"))
    } else {
      check_col_names("data_source", "age")
    }
    
    check_class("age_limit_young", "numeric")
    
    check_class("age_limit_old", "numeric")
    
    
    fulfill_criteria <- FALSE
    
    if (
      test_quantiles == TRUE
    ) {
      fulfill_criteria <-
        min(data_source$upper) < age_limit_young &
        max(data_source$lower) > age_limit_old
    } else {
      fulfill_criteria <-
        min(data_source$age) < age_limit_young &
        max(data_source$age) > age_limit_old
    }
    
    return(fulfill_criteria)
  }