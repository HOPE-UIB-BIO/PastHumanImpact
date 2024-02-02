#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                     Predictor models
#                   Prepare data to fit
#
#                   O. Mottl, V.A. Felde
#                         2023
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
    data_predictors,
    by = "dataset_id"
  ) %>%
  tidyr::unnest(data_merge) %>%
  tidyr::pivot_longer(
    cols = -c(dataset_id, region, climatezone, age),
    values_to = "value",
    names_to = "variable"
  ) %>%
  tidyr::drop_na(value)


if (
  isTRUE(verbose)
) {
  data_to_fit %>%
    dplyr::select(variable, value) %>%
    split(.$variable) %>%
    purrr::map(~ summary(.x))
}

valid_climate_zones_by_n_records <-
  data_merge %>%
  dplyr::group_by(region, climatezone) %>%
  dplyr::distinct(dataset_id) %>%
  dplyr::summarise(
    .groups = "drop",
    n_records = dplyr::n()
  ) %>%
  dplyr::filter(
    n_records >= min_n_records_per_climate_zone # [config]
  )

data_filter_by_climatezone <-
  data_merge %>%
  dplyr::inner_join(
    valid_climate_zones_by_n_records,
    by = c("region", "climatezone")
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
  data_to_fit %>%
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
  use_sha = FALSE
)
