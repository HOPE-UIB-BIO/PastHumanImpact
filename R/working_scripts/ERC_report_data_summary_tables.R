#----------------------------#
# Summary tables of data properties ----
#----------------------------#

library(here)
source("R/00_Config_file.R")

# load data 
data_filtered <- 
  targets::tar_read(
    name = data_assembly_filtered,
    store = external_storage_targets
    ) #1377

options(max.print = 9999)

# Data summary ----

# Sequence level ----
estimates_detailed <-
  data_filtered %>%
  dplyr::filter(!region == "Africa") %>% 
  dplyr::select(
    dataset_id,
    long,
    lat,
    depositionalenvironment,
    region,
    ecozone_koppen_5,
    pollen_percentage,
    counts_harmonised,
    chron_control_format,
    levels
    ) %>%
  
  dplyr::mutate(
    n_taxa_harmonised_per_sequence =
      purrr::map_dbl(
        counts_harmonised,
        ~ .x %>%
          dplyr::select(-sample_id) %>%
          ncol(.)
        ),
    n_sample_filtered_per_sequence =
      purrr::map_dbl(
        counts_harmonised,
        ~ nrow(.x)
        ),
    n_chron_control_filtered_per_sequence =
      purrr::map_dbl(
        chron_control_format,
        ~ nrow(.x)
        ),
    
    chronology_age_per_sample =
      purrr::map(
        chron_control_format,
        ~ .x %>%
          dplyr::select(chroncontrolage)
        ),
    
    chronology_age_error_per_sample =
      purrr::map(
        chron_control_format,
        ~ .x %>%
          dplyr::select(error)
        ),
    
    pollen_pol_counts_per_sample =
      purrr::map(
        counts_harmonised,
        ~ .x %>%
          dplyr::select(-sample_id) %>%
          rowSums(.)
        ),
    
    calibrated_age_per_sample =
      purrr::map(
        levels,
        ~ .x %>%
          dplyr::select(age)
        ),
    
    calibrated_age_uncertainty_per_sample =
      purrr::map(
        levels,
        ~ .x %>%
          dplyr::mutate(
            age_uncertainty_per_sample = (lower - upper) / 2) %>%
          dplyr::select(age_uncertainty_per_sample)
        ),
    
    max_pol_counts_per_sequence =
      purrr::map_dbl(
        pollen_pol_counts_per_sample,
        ~ round(max(.x), 0
                )
        ),
    min_pol_counts_per_sequence =
      purrr::map_dbl(
        pollen_pol_counts_per_sample,
        ~ round(min(.x), 0
                )
        ),
    mean_pol_counts_per_sequence =
      purrr::map_dbl(
        pollen_pol_counts_per_sample,
        ~ round(mean(.x), 0
                )
        ),
    
    max_chronology_age_per_sequence =
      purrr::map_dbl(
        chronology_age_per_sample,
        ~ round(max(.x$chroncontrolage), 2
                )
        ),
    min_chronology_age_per_sequence =
      purrr::map_dbl(
        chronology_age_per_sample,
        ~ round(min(.x$chroncontrolage), 2
                )
        ),
    mean_chronology_age_per_sequence =
      purrr::map_dbl(
        chronology_age_per_sample,
        ~ round(mean(.x$chroncontrolage), 2
                )
        ),
    
    max_chronology_age_error_per_sequence =
      purrr::map_dbl(
        chronology_age_error_per_sample,
        ~ round(max(.x$error), 2
                )
        ),
    min_chronology_age_error_per_sequence =
      purrr::map_dbl(
        chronology_age_error_per_sample,
        ~ round(min(.x$error), 2
                )
        ),
    mean_chronology_age_error_per_sequence =
      purrr::map_dbl(
        chronology_age_error_per_sample,
        ~ round(mean(.x$error), 2
                )
        ),
    
    max_calibrated_age_per_sequence =
      purrr::map_dbl(
        calibrated_age_per_sample,
        ~ round(max(.x$age), 2
                )
        ),
    min_calibrated_age_per_sequence =
      purrr::map_dbl(
        calibrated_age_per_sample,
        ~ round(min(.x$age), 2
                )
        ),
    mean_calibrated_age_per_sequence =
      purrr::map_dbl(
        calibrated_age_per_sample,
        ~ round(mean(.x$age), 2
                )
        ),
    calibrated_age_range_per_sequence =
      (max_calibrated_age_per_sequence - min_calibrated_age_per_sequence),
    
    max_calibrated_age_uncertainty_per_sequence =
      purrr::map_dbl(calibrated_age_uncertainty_per_sample,
                     ~ round(max(.x$age_uncertainty_per_sample), 2
                             )
                     ),
    min_calibrated_age_uncertainty_per_sequence =
      purrr::map_dbl(
        calibrated_age_uncertainty_per_sample,
        ~ round(min(.x$age_uncertainty_per_sample), 2
                )
        ),
    mean_calibrated_age_uncertainty_per_sequence =
      purrr::map_dbl(
        calibrated_age_uncertainty_per_sample,
        ~ round(mean(.x$age_uncertainty_per_sample), 2
                )
        )
    )

estimates_to_be_saved <-
  estimates_detailed %>%
  dplyr::select(
    -c(
      counts_harmonised,
      chron_control_format,
      levels,
      chronology_age_per_sample,
      chronology_age_error_per_sample,
      pollen_pol_counts_per_sample,
      calibrated_age_per_sample,
      calibrated_age_uncertainty_per_sample
      )
    )
write_csv(estimates_to_be_saved,
          file = paste(
            "Data_summary_outputs/Table/",
            "Data_summary_sequence_level_210823.csv",
            sep = ""
            )
          )

# Continent level ----
estimates_continent_level <-
  estimates_to_be_saved %>%
  dplyr::select(
    -c(
      dataset_id,
      long,
      lat,
      depositionalenvironment,
      ecozone_koppen_5,
      pollen_percentage
    )
  ) %>%
  dplyr::group_by(region) %>%
  
  dplyr::mutate(
    max_n_taxa_harmonised_per_continent = max(n_taxa_harmonised_per_sequence),
    min_n_taxa_harmonised_per_continent = min(n_taxa_harmonised_per_sequence),
    mean_n_taxa_harmonised_per_continent = 
      round(mean(n_taxa_harmonised_per_sequence),0
            ),
    max_n_sample_filtered_per_continent = max(n_sample_filtered_per_sequence),
    min_n_sample_filtered_per_continent = min(n_sample_filtered_per_sequence),
    mean_n_sample_filtered_per_continent = 
      round(mean(n_sample_filtered_per_sequence), 0
            ),
    max_n_chron_control_filtered_per_continent = 
      max(n_chron_control_filtered_per_sequence),
    min_n_chron_control_filtered_per_continent = 
      min(n_chron_control_filtered_per_sequence),
    mean_n_chron_control_filtered_per_continent =
      round(mean(n_chron_control_filtered_per_sequence), 0
            ),
    max_pol_counts_per_continent = max(max_pol_counts_per_sequence),
    min_pol_counts_per_continent = min(min_pol_counts_per_sequence),
    mean_pol_counts_per_continent = round(mean(mean_pol_counts_per_sequence), 0
                                          ),
    max_chronology_age_per_continent = max(max_chronology_age_per_sequence),
    min_chronology_age_per_continent = min(min_chronology_age_per_sequence),
    mean_chronology_age_per_continent = mean(mean_chronology_age_per_sequence),
    
    max_chronology_age_error_per_continent = 
      max(max_chronology_age_error_per_sequence),
    min_chronology_age_error_per_continent = 
      min(min_chronology_age_error_per_sequence),
    mean_chronology_age_error_per_continent = 
      mean(mean_chronology_age_error_per_sequence),
    max_calibrated_age_per_continent = max(max_calibrated_age_per_sequence),
    min_calibrated_age_per_continent = min(min_calibrated_age_per_sequence),
    mean_calibrated_age_per_continent = mean(mean_calibrated_age_per_sequence),
    max_calibrated_age_uncertainty_per_continent =
      max(max_calibrated_age_uncertainty_per_sequence),
    min_calibrated_age_uncertainty_per_continent =
      min(min_calibrated_age_uncertainty_per_sequence),
    mean_calibrated_age_uncertainty_per_continent =
      mean(mean_calibrated_age_uncertainty_per_sequence),
    mean_calibrated_age_range_per_continent = 
      mean(calibrated_age_range_per_sequence)
    ) %>%
  
  dplyr::select(
    -c(
      n_taxa_harmonised_per_sequence,
      n_sample_filtered_per_sequence,
      n_chron_control_filtered_per_sequence,
      max_pol_counts_per_sequence,
      min_pol_counts_per_sequence,
      mean_pol_counts_per_sequence,
      max_chronology_age_per_sequence,
      min_chronology_age_per_sequence,
      mean_chronology_age_per_sequence,
      max_chronology_age_error_per_sequence,
      min_chronology_age_error_per_sequence,
      mean_chronology_age_error_per_sequence,
      max_calibrated_age_per_sequence,
      min_calibrated_age_per_sequence,
      mean_calibrated_age_per_sequence,
      calibrated_age_range_per_sequence,
      max_calibrated_age_uncertainty_per_sequence,
      min_calibrated_age_uncertainty_per_sequence,
      mean_calibrated_age_uncertainty_per_sequence
      )
    ) %>%
  distinct()

write_csv(
  estimates_continent_level,
  file  = paste(
    "Data_summary_outputs/Table/",
    "Data_summary_continent_level_210823.csv",
    sep = ""
  )
)

# Chroncontrol type ----
chron_type <-
  estimates_detailed %>%
  dplyr::select(region, chron_control_format) %>%
  dplyr::mutate(chroncontroltype = 
                  purrr::map(
                    chron_control_format,
                    ~ .x %>%
                      dplyr::select(chroncontroltype)
                    )
                ) %>%
  dplyr::select(region, chroncontroltype) %>%
  tidyr::unnest(chroncontroltype) %>% 
  dplyr::group_by(region) %>%
  dplyr::distinct() %>%
  dplyr::arrange(region, chroncontroltype) %>%
  dplyr::ungroup()

write_csv(chron_type,
          file = paste(
            "Data_summary_outputs/Table/",
            "Data_summary_chroncontrol_types_continent_level_210823.csv",
            sep = "")
          )

dep_env <-
  estimates_detailed %>%
  dplyr::select(region, depositionalenvironment) %>%
  dplyr::group_by(region) %>%
  dplyr::distinct() %>%
  dplyr::arrange(region, depositionalenvironment) %>%
  dplyr::ungroup()
write_csv(dep_env,
          file = paste(
            "Data_summary_outputs/Table/",
            "Data_summary_depositional_environment_continent_elvel_210823.csv",
            sep = ""
            )
          )
