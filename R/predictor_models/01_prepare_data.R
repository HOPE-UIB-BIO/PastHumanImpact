#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                     Predictor models
#                   Prepare data to fit
#
#                   O. Mottl, V.A. Felde
#                         2024
#
#----------------------------------------------------------#


#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

# Load configuration
source(
  here::here(
    "R/project/00_Config_file.R"
  )
)

source(
  here::here(
    "R/project/02_meta_data.R"
  )
)

verbose <- FALSE

#----------------------------------------------------------#
# 1. Load data -----
#----------------------------------------------------------#

data_predictors <-
  targets::tar_read(
    name = "data_predictors",
    store = paste0(
      data_storage_path,
      "_targets_data/pipeline_predictors"
    )
  )

if (
  isTRUE(verbose)
) {
  data_predictors %>%
    tidyr::unnest(data_merge) %>%
    summary()
}


#----------------------------------------------------------#
# 2. Data wrangling -----
#----------------------------------------------------------#

data_pre_filter <-
  get_data_filtered(
    data_source = data_predictors,
    data_meta = data_meta,
    age_from = 0,
    age_to = 8500,
    remove_private = TRUE
  )


data_merge <-
  dplyr::inner_join(
    regions, # loaded from 02_meta_data.R
    climate_zones, # loaded from 02_meta_data.R
    by = "dataset_id"
  ) %>%
  dplyr::filter(
    region != "Africa"
  ) %>%
  dplyr::inner_join(
    data_pre_filter,
    by = "dataset_id"
  ) %>%
  tidyr::unnest(data_merge) %>%
  tidyr::pivot_longer(
    cols = -c(dataset_id, region, climatezone, age),
    values_to = "value",
    names_to = "variable"
  ) %>%
  tidyr::drop_na(value)

data_valid_variables <-
  data_merge %>%
  dplyr::filter(
    variable %in% c(
      "temp_annual",
      "temp_cold",
      "prec_summer",
      "prec_win",
      "bi",
      "fi", "fc", "ec", "ei", "cc", "es",
      "weak", "medium", "strong",
      "spd"
    )
  ) %>%
  # There is only certain combination of variables and regions possible.
  # Filter out the rest
  dplyr::mutate(
    invalid_variable = dplyr::case_when(
      .default = FALSE,
      region == "Asia" & variable %in% c(
        "weak", "medium", "strong", "ec", "cc", "es"
      ) ~ TRUE,
      region == "Europe" & variable %in% c(
        "weak", "medium", "strong", "ei", "es"
      ) ~ TRUE,
      region == "North America" & variable %in% c(
        "weak", "medium", "strong", "fi", "ec", "ei", "cc"
      ) ~ TRUE,
      region == "Latin America" & variable %in% c(
        "medium", "fi", "fc", "ec", "ei", "cc", "es"
      ) ~ TRUE,
      region == "Oceania" & variable %in% c(
        "fi", "fc", "ec", "ei", "cc", "es"
      ) ~ TRUE,
    )
  ) %>%
  dplyr::filter(!invalid_variable) %>%
  dplyr::select(-invalid_variable)

if (
  isTRUE(verbose)
) {
  data_valid_variables %>%
    dplyr::select(variable, value) %>%
    split(.$variable) %>%
    purrr::map(~ summary(.x))
}

data_filter_by_age <-
  data_valid_variables %>%
  dplyr::mutate(
    is_spd = dplyr::case_when(
      .default = FALSE,
      variable == "spd" ~ TRUE
    )
  ) %>%
  dplyr::mutate(
    is_valid_age = dplyr::case_when(
      .default = TRUE,
      is_spd == TRUE & age < 2000 ~ FALSE
    )
  ) %>%
  dplyr::filter(
    is_valid_age == TRUE
  ) %>%
  dplyr::select(-c(is_spd, is_valid_age))

valid_climate_zones_by_n_records <-
  data_filter_by_age %>%
  dplyr::group_by(region, climatezone, variable) %>%
  dplyr::distinct(dataset_id) %>%
  dplyr::summarise(
    .groups = "drop",
    n_records = dplyr::n()
  ) %>%
  dplyr::filter(
    n_records >= min_n_records_per_climate_zone # [config]
  )

data_filter_by_climatezone <-
  data_filter_by_age %>%
  dplyr::inner_join(
    valid_climate_zones_by_n_records,
    by = c("region", "climatezone", "variable")
  )

data_constant_variables <-
  data_filter_by_climatezone %>%
  dplyr::distinct(region, climatezone, variable) %>%
  dplyr::mutate(
    is_it_constant = purrr::pmap_lgl(
      .progress = TRUE,
      .l = list(region, climatezone, variable),
      .f = ~ {
        data_filter_by_climatezone %>%
          dplyr::filter(
            region == ..1 &
              climatezone == ..2 &
              variable == ..3
          ) %>%
          dplyr::distinct(value) %>%
          nrow() == 1
      }
    )
  ) %>%
  dplyr::filter(is_it_constant) %>%
  dplyr::select(-is_it_constant)

# Save constant variables for later visualisations
data_filter_by_climatezone %>%
  dplyr::inner_join(
    data_constant_variables,
    by = c("region", "climatezone", "variable")
  ) %>%
  dplyr::distinct(
    region, climatezone, variable, age, value
  ) %>%
  RUtilpol::save_latest_file(
    object_to_save = .,
    file_name = "predictor_models_data_constant",
    dir = paste0(
      data_storage_path,
      "Data/Predictor_models/"
    ),
    prefered_format = "rds",
    use_sha = TRUE
  )

# Remove constant variables
data_filter_out_constant <-
  data_filter_by_climatezone %>%
  dplyr::anti_join(
    data_constant_variables,
    by = c("region", "climatezone", "variable")
  )

data_prepared <-
  data_filter_out_constant %>%
  dplyr::select(-n_records)

#----------------------------------------------------------#
# 3. Visualisation -----
#----------------------------------------------------------#

if (
  isTRUE(verbose)
) {
  summary(data_prepared)

  data_prepared %>%
    dplyr::group_by(region) %>%
    tidyr::nest() %>%
    purrr::chuck("data") %>%
    purrr::map(
      .f = ~ .x %>%
        ggplot2::ggplot(
          ggplot2::aes(
            x = value
          )
        ) +
        ggplot2::geom_histogram(
          bins = 30
        ) +
        ggplot2::facet_wrap(
          ~variable,
          scales = "free"
        )
    ) %>%
    cowplot::plot_grid(plotlist = .)
}

#----------------------------------------------------------#
# 4. Save -----
#----------------------------------------------------------#

RUtilpol::save_latest_file(
  object_to_save = data_prepared,
  file_name = "predictor_models_data_prepared",
  dir = paste0(
    data_storage_path,
    "Data/Predictor_models/"
  ),
  prefered_format = "rds",
  use_sha = TRUE
)
