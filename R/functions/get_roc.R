# function to calculate RoC of the prepared pollen data
# require package RRatepol (see link[])

get_roc <- function(data_pollen, 
                    smoothing_method,
                    min_points_smoothing,
                    max_points_smoothing,
                    age_range_smoothing,
                    working_units_selection,
                    size_of_bin,
                    n_mowing_windows,
                    which_level_select_in_bin,
                    n_rand,
                    n_individuals_to_standardise,
                    transformation_coef,
                    age_max, 
                    ...) {

  data_work_roc <-
  data_pollen %>%
  dplyr::mutate(
    PAP_roc = purrr::pmap(
      .l = list(
        pollen_counts, # ..1
        pollen_percentages, # ..2
        orig_pollen_percentage, # ..3
        levels, # ..4
        age_uncertainty_matrix, # ..5
        dataset_id # ..6
      ),
      .f = ~ {
        current_env <- environment()
        
        message(
          msg = paste("dataset", ..6)
        )
        
        if (
          ..3 == TRUE
        ) {
          data_sel <- ..2 %>%
            dplyr::mutate(
              dplyr::across(
                tidyselect:::where(is.numeric), ~ .x / 100
              )
            )
        } else {
          data_sel <- ..1
        }
        
        try(
          roc_res <-
            RRatepol::fc_estimate_RoC(
              data_source_community = data_sel,
              data_source_age = ..4,
              age_uncertainty = ..5,
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
              standardise = !..3,
              N_individuals = n_individuals_to_standardise,
              tranform_to_proportions = !..3,
              DC = transformation_coef, 
              interest_threshold = age_max, 
              time_standardisation = size_of_bin 
            )  %>% 
            RRatepol::fc_detect_peak_points(
              data_source = .,
              sel_method = "trend_linear",
              sd_threshold = 2
              
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

#----------------------------------------------------------#
# 4. Save -----
#----------------------------------------------------------#

data_roc <-
  data_work_roc %>%
  dplyr::select(dataset_id, PAP_roc)

# readr::write_rds(
#   data_roc,
#   here::here(
#     "Data/Processed/RoC/Data_roc_2022-07-31.rds"
#   ),
#   compress = "gz"
# )

return(data_roc)
}