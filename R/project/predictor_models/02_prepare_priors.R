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

data_to_fit <-
  RUtilpol::get_latest_file(
    file_name = "predictor_models_data_to_fit",
    dir = paste0(
      data_storage_path,
      "Data/Predictor_models/"
    )
  )


#----------------------------------------------------------#
# 2. make formulas -----
#----------------------------------------------------------#

varivale_config <-
  tibble::tribble(
    ~variable, ~errro_family,
    "spd", "Gamma",
    "fi", "binomial",
    "fc", "binomial",
    "ec", "binomial",
    "ei", "binomial",
    "es", "binomial",
    "cc", "binomial",
    "weak", "binomial",
    "medium", "binomial",
    "strong", "binomial",
    "temp_annual", "gaussian",
    "temp_cold", "gaussian",
    "prec_annual", "Gamma",
    "prec_summer", "Gamma",
    "prec_win", "Gamma",
  )

# estimate the average number of records in climate zone
avg_n_groups <-
  data_to_fit %>%
  dplyr::group_by(region, climatezone) %>%
  dplyr::distinct(dataset_id) %>%
  dplyr::count() %>%
  purrr::chuck("n") %>%
  mean() %>%
  round()


hgam_formula <-
  as.formula(
    get_hgam_formula(
      y_var = "variable",
      x_var = "age",
      group_var = "dataset_id",
      sel_k = 10,
      n_groups = avg_n_groups,
    )
  )


#----------------------------------------------------------#
# 3. Estimate priors -----
#----------------------------------------------------------#

data_priors <-
  varivale_config %>%
  dplyr::mutate(
    prior = purrr::map2(
      .progress = TRUE,
      .x = variable,
      .y = errro_family,
      .f = ~ brms::get_prior(
        formula = brms::bf(hgam_formula),
        data = data_to_fit %>%
          dplyr::filter(variable == .x),
        family = .y,
      ),
    )
  )

#----------------------------------------------------------#
# 4. Save priors -----
#----------------------------------------------------------#

RUtilpol::save_latest_file(
  object_to_save = data_priors,
  file_name = "predictor_models_priors",
  dir = paste0(
    data_storage_path,
    "Data/Predictor_models/"
  ),
  prefered_format = "rds",
  use_sha = FALSE
)
