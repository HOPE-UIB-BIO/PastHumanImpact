#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                     Predictor models
#                       Prepare priors
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


#----------------------------------------------------------#
# 1. Load data -----
#----------------------------------------------------------#

data_prepared <-
  RUtilpol::get_latest_file(
    file_name = "predictor_models_data_prepared",
    dir = paste0(
      data_storage_path,
      "Data/Predictor_models/"
    )
  )

#----------------------------------------------------------#
# 2. estimte records per climatezone -----
#----------------------------------------------------------#

data_climate_zones_filtered <-
  data_prepared %>%
  dplyr::group_by(region, climatezone) %>%
  dplyr::distinct(dataset_id) %>%
  dplyr::summarise(
    .groups = "drop",
    n_records = dplyr::n()
  ) %>%
  dplyr::filter(
    n_records >= min_n_records_per_climate_zone # [config]
  )

data_with_valid_climate_zones <-
  data_prepared %>%
  dplyr::inner_join(
    data_climate_zones_filtered,
    by = dplyr::join_by("region", "climatezone")
  )


#----------------------------------------------------------#
# 2. make formulas -----
#----------------------------------------------------------#

# set error family for each variable
varivale_config <-
  tibble::tribble(
    ~variable, ~error_family,
    "spd", "brms::hurdle_gamma(link = 'identity')",
    "fi", "stats::binomial(link='logit')",
    "fc", "stats::binomial(link='logit')",
    "ec", "stats::binomial(link='logit')",
    "ei", "stats::binomial(link='logit')",
    "es", "stats::binomial(link='logit')",
    "cc", "stats::binomial(link='logit')",
    "weak", "stats::binomial(link='logit')",
    "medium", "stats::binomial(link='logit')",
    "strong", "stats::binomial(link='logit')",
    "temp_annual", "stats::gaussian(link='identity')",
    "temp_cold", "stats::gaussian(link='identity')",
    "prec_annual", "brms::hurdle_gamma(link = 'log')",
    "prec_summer", "brms::hurdle_gamma(link = 'log')",
    "prec_win", "brms::hurdle_gamma(link = 'log')",
  )

data_valid_variables <-
  data_with_valid_climate_zones %>%
  dplyr::inner_join(
    varivale_config,
    by = "variable"
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

# add hGAM fromula
data_formulas <-
  data_valid_variables %>%
  tidyr::nest(data_to_fit = c(dataset_id, age, value)) %>%
  dplyr::mutate(
    hgam_formula = purrr::map(
      .x = n_records,
      .f = ~ as.formula(
        get_hgam_formula(
          y_var = "value",
          x_var = "age",
          group_var = "dataset_id",
          sel_k = 10,
          n_groups = .x,
        )
      )
    )
  )


#----------------------------------------------------------#
# 3. Estimate priors -----
#----------------------------------------------------------#

# This is very computationally demanding.
# Use Future to parallelize the process
library(future)
library(furrr)
library(parallelly)
library(brms)

# set up future
future::plan(
  "multisession",
  workers = min(
    c(
      10,
      parallelly::availableCores()
    )
  )
)

# priors are estimated for each variable for the full dataset
data_to_fit <-
  data_formulas %>%
  dplyr::mutate(
    priors = furrr::future_pmap(
      .progress = TRUE,
      .l = list(
        hgam_formula,
        error_family,
        variable
      ),
      .f = ~ brms::get_prior(
        formula = brms::bf(..1),
        family = ..2,
        # we use the full dataset to estimate priors
        data = data_valid_variables %>%
          dplyr::filter(variable == ..3)
      )
    )
  )


#----------------------------------------------------------#
# 4. Save priors -----
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
