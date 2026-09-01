#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#               Analysis evidence manifest
#
#
#                   O. Mottl, V.A. Felde
#                         2024
#
#----------------------------------------------------------#
# Defines the analysis evidence manifest target graph.
# Run with:
#   R/analyses/06_reporting/00_run.R
# Sourcing this script only declares targets; it does not execute them.

#----------------------------------------------------------#
# 0. Configure pipeline -----
#----------------------------------------------------------#

library(here)

source(here::here("R/00_Config_file.R"))

#----------------------------------------------------------#
# 1. Define targets -----
#----------------------------------------------------------#

list(
  # Why: Prepare evidence artifacts so downstream targets share one canonical
  #   dataset.
  targets::tar_target(
    name = "data_evidence_artifacts",
    command = tibble::tribble(
      ~artifact_id, ~description, ~analysis_profile,
      ~source_pipeline, ~public_target, ~path,
      "h1_spatial_controlled_balance",
      "Time-controlled and spatially aggregated H1 balance",
      "spd_spatiotemporal_balance",
      stringr::str_c(
        "02_h1_spatiotemporal_hvarpart/05_spatial_aggregation",
        "spd_human_climate_balance/pipeline.R",
        sep = "/"
      ),
      "table_spatiotemporal_balance_estimates",
      here::here(
        "Outputs", "Figures", "H1", "Spatial", "SPD",
        stringr::str_c(
          "spd",
          "human_climate_balance",
          "zero_truncated_hierarchical_composition",
          "time_and_space_control.pdf",
          sep = "__"
        )
      ),
      "h1_temporal_spatial_composition",
      "Spatially controlled temporal H1 composition",
      "time_slice_spd_spatial_control",
      "02_h1_spatiotemporal_hvarpart/04_spatial_control/spd/pipeline.R",
      "table_spatial_control_zero_truncated_composition",
      here::here(
        "Outputs/Figures/H1/Temporal/HVarPart",
        stringr::str_c(
          "spd_events__human_climate_space__",
          "zero_truncated_hierarchical_composition__space_control.pdf"
        )
      ),
      "pap_temporal_predictions",
      "Validated temporal PAP prediction table",
      "temporal_models",
      "03_temporal_models/06_exports/pipeline.R",
      "file_pap_temporal_predictions",
      here::here(
        "Outputs",
        "Tables",
        "H1",
        "Temporal",
        "PAP_trends",
        "pap__temporal_predictions.csv"
      ),
      "spd_radius_spatial_paired_table",
      "Matched dataset-level strict-radius H1 comparison",
      "spd_radius",
      "91_sensitivity_analyses/spd_radius/pipeline.R",
      "table_spd_radius_spatial_dataset_paired",
      here::here(
        "Outputs/Tables/H1/Spatial/SPD",
        "spd__radius_comparison__dataset_paired.csv"
      ),
      "spd_radius_temporal_paired_table",
      "Matched region-age strict-radius H1 comparison",
      "spd_radius",
      "91_sensitivity_analyses/spd_radius/pipeline.R",
      "table_spd_radius_temporal_region_age_paired",
      here::here(
        "Outputs/Tables/H1/Temporal/HVarPart",
        "spd__radius_comparison__region_age_paired.csv"
      ),
      "spd_radius_spatial_figure",
      "Spatial strict-radius H1 sensitivity figure",
      "spd_radius",
      "91_sensitivity_analyses/spd_radius/pipeline.R",
      "files_spd_radius_sensitivity_figures",
      here::here(
        "Outputs/Figures/H1/Spatial/SPD",
        "spd__radius_comparison__spatial_human_climate_balance.pdf"
      ),
      "spd_radius_temporal_profiles_figure",
      paste(
        "Temporal strict-radius untruncated human-contribution profile",
        "figure"
      ),
      "spd_radius",
      "91_sensitivity_analyses/spd_radius/pipeline.R",
      "files_spd_radius_sensitivity_figures",
      here::here(
        "Outputs/Figures/H1/Temporal/HVarPart",
        stringr::str_c(
          "spd__radius_comparison__temporal_human__",
          "untruncated_hierarchical_contribution__space_control.pdf"
        )
      ),
      "spd_radius_temporal_changes_figure",
      "Temporal strict-radius paired human-contribution change figure",
      "spd_radius",
      "91_sensitivity_analyses/spd_radius/pipeline.R",
      "files_spd_radius_sensitivity_figures",
      here::here(
        "Outputs/Figures/H1/Temporal/HVarPart",
        stringr::str_c(
          "spd__radius_comparison__temporal_human__",
          "untruncated_hierarchical_contribution_change__space_control.pdf"
        )
      ),
      "spd_radius_evidence_manifest",
      "Hashes for all strict-radius source tables and figures",
      "spd_radius",
      "91_sensitivity_analyses/spd_radius/pipeline.R",
      "file_spd_radius_evidence_manifest",
      here::here(
        "Outputs/Tables/Reporting",
        "spd__radius_comparison__evidence_manifest.csv"
      ),
      "spd_radius_sensitivity_response",
      "Rendered reviewer response for strict SPD radius sensitivity",
      "spd_radius",
      "91_sensitivity_analyses/spd_radius/pipeline.R",
      "response_spd_radius_sensitivity.qmd",
      here::here(
        "Manuscript/COMMSENV-25-2408/Reply",
        "response_spd_radius_sensitivity.pdf"
      ),
      "human_event_model_audit",
      "Regional event predictor and chronology model audit",
      "human_event_inclusion",
      "91_sensitivity_analyses/human_event_inclusion/pipeline.R",
      "files_human_event_evidence_tables",
      here::here(
        "Outputs/Tables/H1/Sensitivity/Human_event_inclusion",
        "events__inclusion_comparison__regional_model_audit.csv"
      ),
      "human_event_dataset_matched_table",
      "Three-way matched dataset-level human-event comparison",
      "human_event_inclusion",
      "91_sensitivity_analyses/human_event_inclusion/pipeline.R",
      "files_human_event_evidence_tables",
      here::here(
        "Outputs/Tables/H1/Sensitivity/Human_event_inclusion",
        "events__inclusion_comparison__dataset_matched.csv"
      ),
      "human_event_region_age_matched_table",
      "Three-way matched continent-age human-event comparison",
      "human_event_inclusion",
      "91_sensitivity_analyses/human_event_inclusion/pipeline.R",
      "files_human_event_evidence_tables",
      here::here(
        "Outputs/Tables/H1/Sensitivity/Human_event_inclusion",
        "events__inclusion_comparison__region_age_matched.csv"
      ),
      "human_event_spatial_zero_truncated_balance_figure",
      paste(
        "Spatial regional human-event inclusion sensitivity figure",
        "using zero-truncated hierarchical composition"
      ),
      "human_event_inclusion",
      "91_sensitivity_analyses/human_event_inclusion/pipeline.R",
      "files_human_event_sensitivity_figures",
      here::here(
        "Outputs/Figures/H1/Sensitivity/Human_event_inclusion",
        paste0(
          "events__inclusion_comparison__spatial_",
          "zero_truncated_human_climate_balance.pdf"
        )
      ),
      "human_event_temporal_zero_truncated_profiles_figure",
      "Temporal zero-truncated human-event profile using all data",
      "human_event_inclusion",
      "91_sensitivity_analyses/human_event_inclusion/pipeline.R",
      "files_human_event_sensitivity_figures",
      here::here(
        "Outputs/Figures/H1/Sensitivity/Human_event_inclusion",
        paste0(
          "events__inclusion_comparison__",
          "temporal_zero_truncated_human_share__",
          "space_control__all_data.pdf"
        )
      ),
      "human_event_temporal_zero_truncated_changes_figure",
      "Temporal zero-truncated human-event change using all data",
      "human_event_inclusion",
      "91_sensitivity_analyses/human_event_inclusion/pipeline.R",
      "files_human_event_sensitivity_figures",
      here::here(
        "Outputs/Figures/H1/Sensitivity/Human_event_inclusion",
        paste0(
          "events__inclusion_comparison__",
          "temporal_zero_truncated_human_share_change__",
          "space_control__all_data.pdf"
        )
      ),
      "human_event_evidence_manifest",
      "Hashes for all regional human-event source tables and figures",
      "human_event_inclusion",
      "91_sensitivity_analyses/human_event_inclusion/pipeline.R",
      "file_human_event_evidence_manifest",
      here::here(
        "Outputs/Tables/Reporting",
        "events__inclusion_comparison__evidence_manifest.csv"
      ),
      "human_event_sensitivity_response",
      "Rendered reviewer response for regional human-event sensitivity",
      "human_event_inclusion",
      "91_sensitivity_analyses/human_event_inclusion/pipeline.R",
      "response_human_event_sensitivity.qmd",
      here::here(
        "Manuscript/COMMSENV-25-2408/Reply",
        "response_human_event_sensitivity.pdf"
      )
    )
  ),
  # Why: Materialize evidence manifest so downstream reporting uses an auditable
  #   result.
  targets::tar_target(
    name = "table_evidence_manifest",
    command = build_evidence_manifest(
      data_artifacts = data_evidence_artifacts
    )
  ),
  # Why: Track evidence manifest as a file target so file changes invalidate
  #   downstream results.
  targets::tar_target(
    name = "file_evidence_manifest",
    command = {
      path_output <-
        here::here(
          "Outputs",
          "Tables",
          "Reporting",
          "evidence__manifest.csv"
        )

      dir.create(
        path = dirname(path_output),
        recursive = TRUE,
        showWarnings = FALSE
      )

      readr::write_csv(table_evidence_manifest, path_output)

      path_output
    },
    format = "file"
  )
)
