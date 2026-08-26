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
