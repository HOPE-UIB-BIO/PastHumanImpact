#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                  General temporal models
#                    Audit legacy models
#
#                   O. Mottl, V.A. Felde
#                         2026
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

path_temporal_models <-
  file.path(
    data_storage_path,
    "Temporal_models"
  )
path_model_dir <-
  file.path(
    path_temporal_models,
    "Mods"
  )


#----------------------------------------------------------#
# 1. Select legacy models -----
#----------------------------------------------------------#

model_config <-
  RUtilpol::get_latest_file(
    file_name = "general_model_config_table",
    dir = path_temporal_models
  )

legacy_model_ids <-
  model_config %>%
  dplyr::filter(
    is_model_eligible,
    !need_to_run,
    !need_to_be_evaluated,
    is.na(last_run_id) | !nzchar(last_run_id)
  ) %>%
  dplyr::pull(model_id)

recorded_model_ids <-
  model_config %>%
  dplyr::filter(
    is_model_eligible,
    !need_to_run,
    !need_to_be_evaluated,
    !is.na(last_run_id),
    nzchar(last_run_id)
  ) %>%
  dplyr::pull(model_id)


#----------------------------------------------------------#
# 2. Audit saved models -----
#----------------------------------------------------------#

legacy_model_audit <-
  purrr::map(
    .progress = "Auditing legacy temporal models",
    .x = legacy_model_ids,
    .f = ~ audit_legacy_model_provenance(
      model_id = .x,
      model_dir = path_model_dir
    )
  ) %>%
  dplyr::bind_rows()

recorded_model_audit <-
  purrr::map(
    .progress = "Auditing recorded temporal models",
    .x = recorded_model_ids,
    .f = ~ audit_legacy_model_provenance(
      model_id = .x,
      model_dir = path_model_dir
    )
  ) %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(
    model_seed_source = dplyr::if_else(
      model_provenance_status == "legacy_seeds_recovered",
      "configured_sampling_seed",
      model_seed_source
    ),
    model_provenance_status = dplyr::if_else(
      model_provenance_status == "legacy_seeds_recovered",
      "configured_run_recorded",
      model_provenance_status
    )
  )

model_provenance_audit <-
  dplyr::bind_rows(
    legacy_model_audit,
    recorded_model_audit
  )

RUtilpol::save_latest_file(
  object_to_save = model_provenance_audit,
  file_name = "model_provenance_audit",
  dir = path_temporal_models,
  prefered_format = "csv"
)


#----------------------------------------------------------#
# 3. Update model config -----
#----------------------------------------------------------#

model_config_updated <-
  apply_model_provenance_audit(
    data_config = model_config,
    data_audit = model_provenance_audit
  )

RUtilpol::save_latest_file(
  object_to_save = model_config_updated,
  file_name = "general_model_config_table",
  dir = path_temporal_models,
  prefered_format = "csv"
)
