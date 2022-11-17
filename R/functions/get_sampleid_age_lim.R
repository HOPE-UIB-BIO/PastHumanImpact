#' @title Select levels within the focal time period
#' @param data_source Data.frame with ages of levels
#' @param age_limit Maximum age value, marking the end of interest period for
#' selected site
#' @param current_year_cuttof should cut off levels be beyond current year?
#' @param current_year_cuttof_value year value of current year
#' @param test_quantiles Test quantiles of age prediction?
#' @param bookend Keep one extra level beyond the limit?
#' @return Vector with valid sample_ids
#' @description Exclude levels which are beyond the `age_limit`
#' User can use age quantiles instead of set ages using `test_quantiles` or
#' keep one level beyond the age extra i.e. `bookend`
get_sampleid_age_lim <-
  function(data_source,
           age_limit,
           current_year_cuttof = TRUE,
           current_year_cuttof_value = -76,
           test_quantiles = FALSE,
           bookend = FALSE) {
    current_frame <- sys.nframe()
    
    current_env <- sys.frame(which = current_frame)
    
    check_class("data_source", "data.frame")
    
    check_class("age_limit", "numeric")
    
    check_class("test_quantiles", "logical")
    
    if (
      test_quantiles == TRUE
    ) {
      check_col_names("data_source", "upper")
    } else {
      check_col_names("data_source", "age")
    }
    
    check_class("current_year_cuttof", "logical")
    
    if (
      current_year_cuttof == TRUE
    ) {
      check_class("current_year_cuttof_value", "numeric")
      
      if (
        test_quantiles == TRUE
      ) {
        check_col_names("data_source", "lower")
      }
    }
    
    check_class("bookend", "logical")
    
    if (
      test_quantiles == TRUE
    ) {
      data_work <-
        data_source %>%
        dplyr::mutate(
          exclude = upper > age_limit,
          change = c(0, diff(exclude))
        )
      if (
        current_year_cuttof == TRUE
      ) {
        data_work <-
          data_work %>%
          dplyr::filter(lower > current_year_cuttof_value)
      }
    } else {
      data_work <-
        data_source %>%
        dplyr::mutate(
          exclude = age > age_limit,
          change = c(0, diff(exclude))
        )
      if (
        current_year_cuttof == TRUE
      ) {
        data_work <-
          data_work %>%
          dplyr::filter(age > current_year_cuttof_value)
      }
    }
    
    if (
      bookend == TRUE
    ) {
      data_subset <-
        data_work %>%
        dplyr::filter(exclude == FALSE | change == 1) %>%
        dplyr::select(-c(exclude, change))
    } else {
      data_subset <-
        data_work %>%
        dplyr::filter(exclude == FALSE) %>%
        dplyr::select(-c(exclude, change))
    }
    
    if (
      nrow(data_subset) > 0
    ) {
      data_subset$sample_id %>%
        unique() %>%
        return()
    } else {
      return(NA_character_)
    }
  }