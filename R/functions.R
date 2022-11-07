#' @title Get pollen data from data assembly
#' @description Use data assembly derived from RFossilPol
#' @return For each site a tibble with dataset_id, levels, harmonised pollen counts and pollen percentages

get_data_pollen <- function(file) {
  
  read_rds(file) %>% 
    pluck("data") %>%
    dplyr::select(dataset_id, 
                  levels, 
                  counts_harmonised, 
                  pollen_percentage,
                  age_uncertainty,
                  end_of_interest_period) %>%
    dplyr::mutate(
      pollen_percentages = 
        purrr::map(
          .x = counts_harmonised,
          .f = function(.x) {
            df <-  .x %>%
              column_to_rownames("sample_id")
            percent <- df/rowSums(df) *100
            new <- percent %>% 
              rownames_to_column("sample_id") %>% 
              as_tibble()
            return(new)
          }
        ))
}



#' @title Get site data from data assembly
#' @description Use data assembly derived from RFossilpol
#' @return For each site a tibble with dataset_id, geographical coordinates, altitude, depositional environment, country, region, and ecozone

get_data_site <- function(file) {
  
  read_rds(file) %>% 
    pluck("data") %>%
    dplyr::select(dataset_id, 
                  handle, lat, long,  
                  altitude, depositionalenvironment, 
                  region, ecozone_koppen_5)
}


#' @title Filter pollen sequences of from start of relevant time period 
#' @description Filter samples based on latest age of interest
#' @return the same input data tibble but with filtered pollen data

filter_age_levels <- function(data, late_age_limit = 8.5e3) {
  
  data %>%
    mutate(sample_ids_keep = 
             purrr::map2(levels, 
                         end_of_interest_period,
                        .f = function(.x, .y){
                          sample_id <- .x %>%
                            filter(age < .y) %>%
                            pluck("sample_id")
                        })) %>%
    mutate(across(c(levels, counts_harmonised, pollen_percentages),
                  .fns = ~ 
                    map2(.x, .y = sample_ids_keep,
                          .f = function(.x, .y){
                            .x %>% 
                              filter(sample_id %in% .y)
                          } ))) %>%
    dplyr::select(-sample_ids_keep)
  
}


#' @title Get diversity estimates
#' @description Use pollen assemblage data
#' @param data_pollen use pollen counts if possible
#' @param n_rand number of randomisations
#' @param sel_method selection of approach
#' @return For each site dataset the rarefied estimates of Hill´s effective species numbers (which are the estimated number of taxa (N0), estimated number of equally common taxa (N1), estimated number of equally abundant taxa (N2)), and the associated evenness ratios N1/N0 and N2/N1 
get_diversity <- function(data_pollen, n_rand =  999, sel_method = "taxonomic") {
  
  data_work_diversity <-
    data_pollen %>%
    dplyr::mutate(
      PAP_diversity = purrr::map(
        .x = counts_harmonised,
        .f = ~ REcopol::diversity_estimate(
          data_source = .x,
          sel_method = sel_method,
          rand = n_rand 
        )
      )
    )
  
  data_diversity <-
    data_work_diversity %>%
    dplyr::select(dataset_id, PAP_diversity)
  
  return(data_diversity)

  
}


#' @title Get multivariate regression trees (MRT) 
#' @description Use pollen assemblage data and age/time as constraint
#' @param data_pollen Use pollen percentages
#' @param n_rand number of randomisations
#' @param transformation_coef which distance coefficient to use. Recommended for pollen data is chi-squared distances of pollen percentage with no data transformations  
#' @return For each sequence returns a vector of the zonation, a vector of the change points, and the total number of zones 

get_mrt <- function(data_pollen, n_rand = 999, transformation_coef = "chisq") {
  
  data_work_mrt <-
    data_pollen %>%
    dplyr::mutate(
      PAP_mrt = purrr::map2(
        .x = pollen_percentages,
        .y = levels,
        .f = ~ REcopol::mv_regression_partition(
          data_source_counts = .x,
          data_source_levels = .y,
          rand = n_rand, 
          transformation = transformation_coef 
        )
      )
    )
  
  data_mrt_proc <-
    data_work_mrt %>%
    dplyr::mutate(
      mvrt_partitions = purrr::map(
        .x = PAP_mrt,
        .f = ~ .x %>%
          purrr::pluck("partitions") %>%
          dplyr::rename(MRT_partitions = partition)
      ),
      mvrt_cp = purrr::map(
        .x = PAP_mrt,
        .f = ~ .x %>%
          purrr::pluck("change_points")
      ),
      mvrt_groups_n = purrr::map_dbl(
        .x = PAP_mrt,
        .f = ~ .x %>%
          purrr::pluck("mrt_groups") 
      )
    )
  
  data_mrt <-
    data_mrt_proc %>%
    dplyr::select(dataset_id, PAP_mrt, mvrt_partitions, mvrt_cp, mvrt_groups_n)
  
  return(data_mrt)
}

#' @title Get estimates of pollen compositional turnover with time using Detrended Canonical Correspondence Analysis (DCCA)
#' @description Use pollen assemblage data and age/time as constraint
#' @param data_pollen Use pollen percentages
#' @param sel_method Choose unconstrained or constrained ordination method. The constrained version is DCCA with Canoco 4.5, and it can only be run on the Windows platform, the unconstrained version is run using vegan::decorana() in R and can be run on all platforms
#' @param var_name_pred Name of the constraining variable
#' @param sel_complexity Allow flexibility using second or third order polynomials of the constraining variable
#' @param transform_to_percentage TRUE or FALSE depending on input data, provide configuration for Canoco 4.5
#' @param transformation can be none, square-root or log 
#' @return For each dataset the DCCA results, the weighted average scores for each level for DCCA axis 1 (case_r scores), and total gradient length for DCCA 1  

get_dcca <- function(data_pollen, ...) {
  
  data_dcca <-
    data_pollen %>%
    dplyr::mutate(
      dcca = purrr::map2(
        .x = pollen_percentages,
        .y = levels,
        .f = ~ REcopol::fit_ordination(
          data_source_community = .x,
          data_source_predictors = .y,
          sel_method = "constrained",
          var_name_pred = "age",
          sel_complexity = "poly_2",
          transform_to_percentage = FALSE,
          tranformation = "none"
        )
      )
    )
  
  data_dcca_proc <-
    data_dcca %>%
    dplyr::mutate(
      dcca_scores = purrr::map(
        .x = dcca,
        .f = ~ .x %>% purrr::pluck("case_r")
      ),
      dcca_grad_length = purrr::map_dbl(
        .x = dcca,
        .f = ~ .x %>% purrr::pluck("axis_1_grad_length")
      )
    )

  data_turnover <-
    data_dcca_proc %>%
    dplyr::select(
      dataset_id,
      dcca,
      dcca_scores,
      dcca_grad_length
    )
  
  return(data_turnover)
  
}


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

#' @title Run generalised additive models
#' @description Get all the variables interpolated at the same age levels
#' @param data Use all derived PAP estimates and predictor variables
#' @return For each sequence give new predicted data for all variables at the same age levels

# # tables with names of variables, errors, and dataframes
# vars_table <-
#   tibble::tibble(
#     var_name = c(
#       "n0",
#       "n1",
#       "n2",
#       "dcca_axis_1",
#       
#       "n2_divided_by_n1",
#       "n1_divided_by_n0",
#       "ROC",
#       "Peak"
#     ),
#     sel_error = c(
#       rep("mgcv::Tweedie(p = 1.1)", 4),
#       rep("mgcv::betar(link = 'logit')", 2),
#       "mgcv::Tweedie(p = 1.1)",
#       "stats::quasibinomial(link = 'logit')"
#     ),
#     sel_data = c(
#       rep("data_diversity", 6),
#       rep("data_roc", 2)
#     )
#   )
# 
# max_temporal_k <- 24
