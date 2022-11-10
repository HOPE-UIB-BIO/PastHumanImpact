#' @title Get Rate-of-change estimates 
#' @description Use RRatepol to estimate RoC of pollen sequences
#' @param data_pollen Pollen counts but if not available can use percentages
#' @param smoothing_method
#' @param min_points_smoothing
#' @param age_range_smoothing
#' @param working_units_selection if run in parallel
#' @param size_of_bin in ages
#' @param n_mowing_windows
#' @param which_level_select_in_bin
#' @param n_rand number of randomisations
#' @param n_individuals_to_standardise set to  150 grains
#' @param transformation_coef coefficient distances
#' @return For each sequence the ROC results 
#' 
get_roc <- function(data_pollen, 
                    smoothing_method = "age.w",
                    min_points_smoothing = 5,
                    max_points_smoothing = 9,
                    age_range_smoothing = 500,
                    working_units_selection = "MW",
                    size_of_bin = 500,
                    n_mowing_windows = 5,
                    which_level_select_in_bin = "random",
                    n_rand = 1000,
                    n_individuals_to_standardise = 150,
                    transformation_coef = "chisq",
                    peak_point_method = "trend_linear",
                    sd_for_peak_detection = 2,
                    ...) {
  
  data_work_roc <-
    data_pollen %>%
    dplyr::mutate(
      PAP_roc = purrr::pmap(
        .l = list(
          counts_harmonised, # ..1
          levels, # ..2
          age_uncertainty, # ..3
          pollen_percentage, # ..4
          dataset_id, # ..5
          end_of_period_of_interest # ..6
        ),
        .f = ~ {
          current_env <- environment()
          
          message(
            msg = paste("dataset", ..5)
          )
          
          
          try(
            roc_res <-
              RRatepol::fc_estimate_RoC(
                data_source_community = ..1,
                data_source_age = ..2,
                age_uncertainty = ..3,
                smooth_method = smoothing_method, 
                smooth_N_points = min_points_smoothing, 
                smooth_N_max = max_points_smoothing, 
                smooth_age_range = age_range_smoothing, 
                Working_Units = working_units_selection, 
                bin_size = size_of_bin, 
                Number_of_shifts = n_mowing_windows,
                bin_selection = which_level_select_in_bin, 
                rand = n_rand, 
                use_parallel = TRUE,
                standardise = !..4,
                N_individuals = n_individuals_to_standardise,
                tranform_to_proportions = !..4, 
                DC = transformation_coef, 
                interest_threshold = ..6, 
                time_standardisation = size_of_bin 
              )  %>% 
              RRatepol::fc_detect_peak_points(
                data_source = .,
                sel_method = peak_point_method,
                sd_threshold = sd_for_peak_detection
                
              ),
            silent = TRUE
          )
          
          if (
            !exists("roc_res", envir = current_env)
          ) {
            roc_res <- NA
          }
          
          return(roc_res)
        }
      )
    )
  
  
  return(data_roc)
}