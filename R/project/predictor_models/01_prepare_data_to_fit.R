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

data_to_fit <-
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
  object_to_save = data_to_fit,
  file_name = "predictor_models_data_to_fit",
  dir = paste0(
    data_storage_path,
    "Data/Predictor_models/"
  ),
  prefered_format = "rds",
  use_sha = FALSE
)
