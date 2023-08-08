#----------------------------------------------------------#
#
#
#                     Project name
#
#                Run predictor general trends
#
#
#                    O. Mottl, V. Felde
#                         2023
#
#----------------------------------------------------------#
# Run a general pattern of our data for each predictor per each ecozone
#   For now, only test on Europe and on max of 30 records per ecozone


#----------------------------------------------------------#
# 0. Set up -----
#----------------------------------------------------------#

library(tidyverse)
library(targets)
library(mgcv)

# set configuration for _target storage
targets::tar_config_set(
  store = here::here("_targets1")
)

invisible(
  lapply(
    list.files(
      path = "R/functions",
      pattern = "*.R",
      recursive = TRUE,
      full.names = TRUE
    ),
    source
  )
)

# vector with predictor names
vec_predictors <-
  c(
    "temp_annual",
    "prec_annual",
    "prec_summer",
    "prec_win",
    "temp_cold",
    "spd"
  )

# GAM max `k`
max_temporal_k <- 12

set.seed(1234)

#----------------------------------------------------------#
# 1. Load data -----
#----------------------------------------------------------#
data_for_hvar <-
  targets::tar_read(name = "data_for_hvar")

data_meta <-
  targets::tar_read(name = "data_meta")


#----------------------------------------------------------#
# 2. Data wrangling -----
#----------------------------------------------------------#

data_predictors <-
  data_for_hvar %>%
  tidyr::unnest(data_merge) %>%
  dplyr::select(
    dplyr::all_of(
      c(
        "dataset_id",
        "age",
        vec_predictors
      )
    )
  )

data_ecozone <-
  data_meta %>%
  dplyr::select(
    dplyr::all_of(
      c(
        "dataset_id",
        "region",
        "ecozone_koppen_15"
      )
    )
  )

data_work <-
  dplyr::left_join(
    data_ecozone,
    data_predictors,
    by = "dataset_id"
  )

# temporal subset of data to speed the code development
data_datasets_per_ecozone <-
  data_work %>%
  dplyr::distinct(
    region, ecozone_koppen_15, dataset_id
  )

data_ecozone_n_records <-
  data_datasets_per_ecozone %>%
  dplyr::group_by(region, ecozone_koppen_15) %>%
  dplyr::summarise(
    .groups = "drop",
    n_datasets = n_distinct(dataset_id)
  )

data_valid_ecozones <-
  data_ecozone_n_records %>%
  dplyr::filter(region == "Europe") %>%
  dplyr::filter(n_datasets > 5)

data_datasets_per_ecozone_subset <-
  data_datasets_per_ecozone %>%
  # this will drop unwanted ecozones (small)
  dplyr::inner_join(
    data_valid_ecozones,
    by = dplyr::join_by(region, ecozone_koppen_15)
  ) %>%
  dplyr::group_by(
    region, ecozone_koppen_15
  ) %>%
  # get maximum of 30 records per ecozone
  dplyr::slice_sample(n = 30, replace = FALSE) %>%
  dplyr::ungroup()

data_datasets_per_ecozone_subset %>%
  dplyr::group_by(region, ecozone_koppen_15) %>%
  dplyr::summarise(
    .groups = "drop",
    n_datasets = dplyr::n_distinct(dataset_id)
  ) %>%
  dplyr::arrange(-n_datasets) %>%
  View()

vec_subset_dataset_id <-
  data_datasets_per_ecozone_subset %>%
  purrr::chuck("dataset_id")

data_work_subset <-
  data_work %>%
  dplyr::filter(
    dataset_id %in% vec_subset_dataset_id
  ) %>%
  dplyr::mutate(
    group = ecozone_koppen_15
  )


#----------------------------------------------------------#
# 3. Fit and save hGAMS -----
#----------------------------------------------------------#

# do a small test to get the best family for spd (on small subset of data)
data_family_test <-
  c(
    "mgcv::tw(link = 'log')",
    "mgcv::tw(link = 'sqrt')",
    "mgcv::Tweedie(p = 1.1, link = 'log')",
    "mgcv::Tweedie(p = 1.1, link = 'sqrt')",
    "mgcv::Tweedie(p = 1.5, link = 'log')",
    "mgcv::Tweedie(p = 1.5, link = 'sqrt')",
    "mgcv::Tweedie(p = 1.9, link = 'log')",
    "mgcv::Tweedie(p = 1.9, link = 'sqrt')"
  ) %>%
  rlang::set_names() %>%
  purrr::map(
    .f = ~ fit_and_save_hgam(
      data_raw = data_work_subset,
      sel_group = "Arid_Steppe",
      y_var = "spd",
      group_var = "dataset_id",
      smooth_basis = "tp",
      error_family = .x,
      sel_k = max_temporal_k,
      use_parallel = FALSE,
      verbose = TRUE,
      save = FALSE
    )
  )

purrr::map_dbl(
  .x = data_family_test,
  .f = ~ AIC(.x)
) %>%
  which.min() %>%
  names()

# "mgcv::tw(link = 'log')" seems to be the best

data_pred_errors <-
  tibble::tibble(
    predictor = vec_predictors,
    error_family = c(
      "stats::gaussian(link = 'identity')",
      "stats::Gamma(link = 'log')",
      "stats::Gamma(link = 'log')",
      "stats::Gamma(link = 'log')",
      "stats::gaussian(link = 'identity')",
      "mgcv::tw(link = 'log')"
    )
  )

data_pred_dummy <-
  tidyr::expand_grid(
    ecozone_koppen_15 = unique(data_work_subset$ecozone_koppen_15),
    data_pred_errors
  )

gc(verbose = TRUE)

purrr::pwalk(
  .l = list(
    data_pred_dummy$ecozone_koppen_15,
    data_pred_dummy$predictor,
    data_pred_dummy$error_family
  ),
  .f = ~ fit_and_save_hgam(
    data_raw = data_work_subset,
    sel_group = ..1,
    dir = here::here(
      "Data/Proccesed/Models/hGAM/Predictors"
    ),
    y_var = ..2,
    group_var = "dataset_id",
    smooth_basis = "tp",
    error_family = ..3,
    sel_k = max_temporal_k,
    use_parallel = TRUE,
    verbose = TRUE
  )
)
