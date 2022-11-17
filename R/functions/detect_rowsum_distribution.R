#' @title Test if enough levels fulfil criteria of pollen sum
#' @param data_counts Data.frame with pollen data
#' @param data_levels Data.frame with level ages
#' @param age_limit_young young age limit for selected site
#' @param age_limit_old old age limit for selected site
#' @param target_n_grains preferred number of pollen sum
#' @param percentage_samples percentages of levels, which are in the age limit
#' to be tested if their pollen sum fulfil criteria
#' @param test_quantiles Test quantiles of age prediction?
#' @return TRUE/FALSE (logical)
#' @description Calculate pollen sum of each level and test if it is above
#' target_n_grains. Compare sequence has at least X% (percentage_samples) of
#' levels, within the age period, with enough pollen sum. Level is counted
#' "in" period if any part of his 95th age quantile is between age_limit_young
#' and age_limit_old.
detect_rowsum_distribution <-
  function(data_counts,
           data_levels,
           age_limit_young,
           age_limit_old,
           target_n_grains,
           percentage_samples,
           test_quantiles = FALSE) {
    check_class("data_counts", "data.frame")
    
    check_col_names("data_counts", "sample_id")
    
    check_class("data_levels", "data.frame")
    
    check_col_names("data_levels", "sample_id")
    
    check_class("age_limit_young", "numeric")
    
    check_class("age_limit_old", "numeric")
    
    check_class("target_n_grains", "numeric")
    
    check_class("percentage_samples", "numeric")
    
    check_class("test_quantiles", "logical")
    
    if (
      test_quantiles == TRUE
    ) {
      check_col_names("data_levels", c("lower", "upper"))
    } else {
      check_col_names("data_levels", "age")
    }
    
    data_strip <-
      data_counts %>%
      dplyr::select(-tidyselect::contains("sample_id"))
    
    data_merged <-
      data_counts %>%
      dplyr::mutate(rowsum = rowSums(data_strip)) %>%
      dplyr::select(sample_id, rowsum) %>%
      dplyr::mutate(
        above_threshold = rowsum >= target_n_grains
      ) %>%
      dplyr::inner_join(
        data_levels,
        by = ("sample_id")
      )
    
    check_col_names("data_merged", "above_threshold")
    
    if (
      test_quantiles == TRUE
    ) {
      data_subset <-
        data_merged %>%
        dplyr::filter(lower > age_limit_young) %>%
        dplyr::filter(upper < age_limit_old)
    } else {
      data_subset <-
        data_merged %>%
        dplyr::filter(age > age_limit_young) %>%
        dplyr::filter(age < age_limit_old)
    }
    
    # set default
    fulfill_criteria <- FALSE
    
    if (
      nrow(data_subset) > 0
    ) {
      fulfill_criteria <-
        (mean(data_subset$above_threshold) * 100) > percentage_samples
    }
    
    return(fulfill_criteria)
  }