#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                  General temporal models
#                Prepare model config table
#
#                   O. Mottl, V.A. Felde
#                         2024
#
#----------------------------------------------------------#


#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

source(
  here::here(
    "R/00_Config_file.R"
  )
)

overwrite_table <- FALSE


#----------------------------------------------------------#
# 1. Load data and specs -----
#----------------------------------------------------------#

data_general_model <-
  RUtilpol::get_latest_file(
    file_name = "general_temporal_model_data",
    dir = paste0(
      data_storage_path,
      "Temporal_models/"
    )
  )

data_model_specs <-
  RUtilpol::get_latest_file(
    file_name = "general_temporal_model_specs",
    dir = paste0(
      data_storage_path,
      "Temporal_models/"
    )
  )


#----------------------------------------------------------#
# 2. Create config table -----
#----------------------------------------------------------#

family_key_by_model <-
  data_model_specs %>%
  dplyr::select(model_id, family_key) %>%
  tibble::deframe()

model_config_table <-
  create_model_config_table(
    data_model = data_general_model,
    analysis = "general_temporal",
    family_key = family_key_by_model,
    engine = "brms",
    model_profile = "within_stratum_dataset_fs",
    age_min = 0,
    age_max = 8500,
    timestep = 500,
    min_records = min_n_records_per_climate_zone,
    total_iterations = 3200,
    min_iterations_per_chain = 100,
    max_chains = 4
  ) %>%
  dplyr::select(
    -engine,
    -model_profile
  ) %>%
  dplyr::left_join(
    data_model_specs %>%
      dplyr::select(
        analysis,
        model_id,
        variable,
        region,
        climatezone,
        engine,
        model_profile
      ),
    by = c("analysis", "model_id", "variable", "region", "climatezone")
  ) %>%
  dplyr::relocate(engine, model_profile, .after = family_key)


#----------------------------------------------------------#
# 3. Save table -----
#----------------------------------------------------------#

config_exists <-
  RUtilpol::get_latest_file_name(
    file_name = "general_model_config_table",
    dir = paste0(
      data_storage_path,
      "Temporal_models/"
    )
  ) %>%
  is.na() %>%
  isFALSE()

config_needs_refresh <- FALSE

if (
  isTRUE(config_exists)
) {
  config_current <-
    RUtilpol::get_latest_file(
      file_name = "general_model_config_table",
      dir = paste0(
        data_storage_path,
        "Temporal_models/"
      ),
      verbose = FALSE
    )

  config_needs_refresh <-
    !all(c("output_id", "region", "climatezone") %in% names(config_current)) ||
    !setequal(config_current[["model_id"]], model_config_table[["model_id"]])
}

if (
  isTRUE(overwrite_table) ||
    isFALSE(config_exists) ||
    isTRUE(config_needs_refresh)
) {
  RUtilpol::save_latest_file(
    object_to_save = model_config_table,
    file_name = "general_model_config_table",
    dir = paste0(
      data_storage_path,
      "Temporal_models/"
    ),
    prefered_format = "csv"
  )
}
