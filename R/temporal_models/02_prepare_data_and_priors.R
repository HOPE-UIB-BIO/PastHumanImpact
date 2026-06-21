#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                  General temporal models
#                    Prepare model specs
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


#----------------------------------------------------------#
# 1. Load data -----
#----------------------------------------------------------#

data_general_model <-
  RUtilpol::get_latest_file(
    file_name = "general_temporal_model_data",
    dir = paste0(
      data_storage_path,
      "Temporal_models/"
    )
  )


#----------------------------------------------------------#
# 2. Prepare model specs -----
#----------------------------------------------------------#

data_model_specs <-
  data_general_model %>%
  dplyr::distinct(analysis, variable, region, climatezone) %>%
  dplyr::mutate(
    engine = "brms",
    model_profile = "within_stratum_dataset_fs",
    family_key = purrr::map2_chr(
      .x = variable,
      .y = analysis,
      .f = ~ get_default_model_family_key(
        variable = .x,
        analysis = .y
      )
    ),
    output_id = stringr::str_c(
      analysis,
      variable,
      region,
      climatezone,
      sep = "__"
    ) %>%
      stringr::str_replace_all("[^A-Za-z0-9_]+", "_") %>%
      stringr::str_replace_all("^_|_$", ""),
    model_id = output_id,
    formula_text = purrr::pmap_chr(
      .l = list(model_profile),
      .f = ~ get_hgam_formula(
        model_profile = ..1,
        y_var = "value",
        x_var = "age_ka",
        group_var = "dataset_id",
        stratum_var = "stratum",
        smooth_basis = "cr",
        sel_k = 8,
        stratum_k = 5,
        group_k = 3
      )
    )
  ) %>%
  dplyr::select(
    analysis,
    model_id,
    output_id,
    variable,
    region,
    climatezone,
    family_key,
    engine,
    model_profile,
    formula_text
  ) %>%
  dplyr::arrange(analysis, variable, region, climatezone)


#----------------------------------------------------------#
# 3. Save specs -----
#----------------------------------------------------------#

RUtilpol::save_latest_file(
  object_to_save = data_model_specs,
  file_name = "general_temporal_model_specs",
  dir = paste0(
    data_storage_path,
    "Temporal_models/"
  ),
  prefered_format = "rds",
  use_sha = TRUE
)
