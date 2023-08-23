library(here)
source("R/00_Config_file.R")

# load data 
data_filtered <- 
  targets::tar_read(
    name = data_assembly_filtered,
    store = external_storage_targets
    ) %>% 
  dplyr::filter(!region == "Africa") #1357

options(max.print = 9999)

estimates_detailed <-
  data_filtered %>%
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
  ) %>% 
  dplyr::select(
    -c(
      counts_harmonised,
      chronology_age_per_sample,
      chronology_age_error_per_sample,
      pollen_pol_counts_per_sample,
      calibrated_age_per_sample,
      calibrated_age_uncertainty_per_sample
    )
  )

# Plots of data summary
estimates_detailed$region <-
  factor(
    estimates_detailed$region,
    levels = c(
      'Asia',
      'Europe',
      'North America',
      'Latin America',
      'Oceania'
    )
  )
.get.range.plot <- function(x, VAR){
  x <- 
    estimates_detailed %>% 
    dplyr::mutate_at("dataset_id", as.factor) 
  
  x %>%
    ggplot(
      aes(
        x = dataset_id,
        y = get(VAR),
        fill = ecozone_koppen_5,
        colour = ecozone_koppen_5
      )
    ) +
    geom_col(size = .1) +
    labs(
      x = "",
      y = paste(VAR),
      colour = "Climate zone",
      fill = "Climate zone"
    ) +
    theme_classic() + 
    scale_y_sqrt() +
    scale_colour_manual(values = palette_ecozone) +
    scale_fill_manual(values = palette_ecozone) +
    guides(colour = guide_legend(nrow = 1),
           fill = guide_legend(nrow = 1)) +
    
    facet_wrap(~ region, 
               ncol = 2,
               scales = "free") +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_text(color = "black",
                                 size = 16),
      axis.ticks.x = element_blank(),
      axis.title = element_text(color = "black",
                                size = 18),
      legend.position = "bottom",
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16),
      strip.text.x = element_text(size = 18)
    )
  ggsave(
    file = paste(
      "Data_summary_outputs/Figure/",
      VAR,
      "_210823.tiff", 
      sep = ""
      ),
    dpi = 400,
    compress = "lzw",
    width = 30,
    height = 24,
    unit = "cm"
   )
  }

.get.range.plot(VAR = "n_taxa_harmonised_per_sequence")
.get.range.plot(VAR = "n_sample_filtered_per_sequence")
.get.range.plot(VAR = "n_chron_control_filtered_per_sequence")
.get.range.plot(VAR = "mean_pol_counts_per_sequence")

# Chroncontrol age
.get.chronology.age.range.plot <- function(x, VAR) {
  x <-
    estimates_detailed %>%
    dplyr::select(
      dataset_id,
      region,
      ecozone_koppen_5,
      chron_control_format
      ) %>%
    dplyr::mutate(
      dataset_id = as.factor(dataset_id),
      chron_control_format =
        purrr::map(
          chron_control_format,
          ~ .x %>%
            dplyr::mutate_at(
              "chroncontrolid", 
              as.character
              )
          )
      ) %>%
    tidyr::unnest(chron_control_format) %>%
    dplyr::rename(
      chronology_age_uncalibrated_per_sequence = chroncontrolage,
      chronology_age_error_per_sequence = error
      )
  x %>%
    ggplot(
      aes(
        x = dataset_id,
        y = get(VAR),
        fill = ecozone_koppen_5,
        colour = ecozone_koppen_5
        )
      ) +
    geom_line(size = 1) +
    labs(
      x = "",
      y = paste(VAR),
      colour = "Climate zone",
      fill = "Climate zone"
      ) +
    theme_classic() +
    #scale_y_sqrt() +
    scale_colour_manual(values = palette_ecozone) +
    scale_fill_manual(values = palette_ecozone) +
    guides(
      colour = guide_legend(nrow = 1),
      fill = guide_legend(nrow = 1)
      ) +
    
    facet_wrap(~ region,
               ncol = 2,
               scales = "free"
               ) +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_text(color = "black",
                                 size = 16),
      axis.ticks.x = element_blank(),
      axis.title = element_text(color = "black",
                                size = 18),
      legend.position = "bottom",
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16),
      strip.text.x = element_text(size = 18)
      )
  ggsave(
    file = 
      paste(
        "Data_summary_outputs/Figure/", 
        VAR, 
        "_210823.tiff", 
        sep = ""
        ),
    dpi = 400,
    compress = "lzw",
    width = 30,
    height = 24,
    unit = "cm"
  )
}

.get.chronology.age.range.plot(VAR = "chronology_age_uncalibrated_per_sequence")
.get.chronology.age.range.plot(VAR = "chronology_age_error_per_sequence")


# Calibrated age
.get.calibrated.age.range.plot <- function(x, VAR) {
  x <-
    estimates_detailed %>%
    dplyr::select(
      dataset_id,
      region,
      ecozone_koppen_5,
      levels) %>%
    tidyr::unnest(levels) %>%
    dplyr::mutate(
      dataset_id = as.factor(dataset_id),
      "calibrated_age_per_sequence (cal yr BP)" = age,
      "calibrated_age_uncertainty_per_sequence (yrs)" = ((lower - upper)/2)
      )
  x %>%
    ggplot(
      aes(
        x = dataset_id,
        y = get(VAR),
        fill = ecozone_koppen_5,
        colour = ecozone_koppen_5
        )
      ) +
    geom_line(size = 1) +
    labs(
      x = "",
      y = paste(VAR),
      colour = "Climate zone",
      fill = "Climate zone"
      ) +
    theme_classic() +
    #scale_y_sqrt() +
    scale_colour_manual(values = palette_ecozone) +
    scale_fill_manual(values = palette_ecozone) +
    guides(colour = guide_legend(nrow = 1),
           fill = guide_legend(nrow = 1)
           ) +
    facet_wrap(~ region,
                ncol = 2,
                scales = "free") +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_text(color = "black",
                                 size = 16),
      axis.ticks.x = element_blank(),
      axis.title = element_text(color = "black",
                                size = 18),
      legend.position = "bottom",
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16),
      strip.text.x = element_text(size = 18)
      )
  ggsave(
    file = 
      paste("Data_summary_outputs/Figure/", 
            VAR, 
            "_210823.tiff", 
            sep = ""
            ),
    dpi = 400,
    compress = "lzw",
    width = 30,
    height = 24,
    unit = "cm"
  )
 }

.get.calibrated.age.range.plot(VAR = "calibrated_age_per_sequence (cal yr BP)")
.get.calibrated.age.range.plot(VAR = "calibrated_age_uncertainty_per_sequence (yrs)")

