#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#               Matched SPD radius products
#
#
#                   O. Mottl, V.A. Felde
#                         2026
#
#----------------------------------------------------------#
# Defines the matched 250 km and 500 km SPD target graph.
# Run with:
#   R/analyses/01_data_preparation/00_run.R
# Sourcing this script only declares targets; it does not execute them.

#----------------------------------------------------------#
# 0. Configure pipeline -----
#----------------------------------------------------------#

library(here)

source(here::here("R/00_Config_file.R"))

runner_data_preparation <-
  "R/analyses/01_data_preparation/00_run.R"

#----------------------------------------------------------#
# 1. Define targets -----
#----------------------------------------------------------#

list(
  # Why: Define all scientific SPD settings once and fingerprint them with the
  #   source files in the public provenance table.
  targets::tar_target(
    name = "config_spd_radius",
    command = list(
      radii_km = rlang::set_names(c(250L, 500L)),
      polygon_buffer_degrees = 10,
      age_from = min_age,
      age_to = max_age,
      smoothing_years = 100,
      minimum_dates = 50L,
      normalise_to_one = FALSE,
      calibration_curves = c(
        "intcal20",
        "SHCal20",
        "mixed_curve20"
      )
    )
  ),
  # Why: Track metadata as a file target so coordinate, curve, or geography
  #   changes invalidate the matched products.
  targets::tar_target(
    name = "file_spd_radius_metadata",
    command = resolve_latest_file_path(
      file_name = "data_meta",
      dir = file.path(data_storage_path, "Assembly")
    ),
    format = "file"
  ),
  # Why: Track the fixed radiocarbon snapshot required by the sensitivity.
  targets::tar_target(
    name = "file_spd_radius_c14",
    command = file.path(
      data_storage_path,
      "C14",
      "data_rc_2022-11-29.rds"
    ),
    format = "file"
  ),
  # Why: Track the historical strict 250 km product for a read-only regression
  #   comparison without replacing it.
  targets::tar_target(
    name = "file_spd_radius_reference_250",
    command = resolve_latest_file_path(
      file_name = "data_spd_250",
      dir = file.path(data_storage_path, "SPD")
    ),
    format = "file"
  ),
  # Why: Track the historical fallback product for a read-only compatibility
  #   audit without replacing the canonical predictor input.
  targets::tar_target(
    name = "file_spd_radius_reference_fallback",
    command = resolve_latest_file_path(
      file_name = "data_spd_combine",
      dir = file.path(data_storage_path, "SPD")
    ),
    format = "file"
  ),
  # Why: Load metadata once for filtering and geographic coverage summaries.
  targets::tar_target(
    name = "data_spd_radius_metadata",
    command = resolve_file_path(file_spd_radius_metadata)
  ),
  # Why: Load the fixed C14 snapshot once before deterministic spatial
  #   filtering.
  targets::tar_target(
    name = "data_spd_radius_c14",
    command = resolve_file_path(file_spd_radius_c14)
  ),
  # Why: Build one common search envelope for each dataset before calculating
  #   either archaeological radius.
  targets::tar_target(
    name = "data_spd_radius_polygons",
    command = build_spatial_polygons(
      data_source = data_spd_radius_metadata,
      distance_buffer =
        config_spd_radius[["polygon_buffer_degrees"]]
    )
  ),
  # Why: Filter the shared radiocarbon snapshot once so both radii use exactly
  #   the same dates and calibration-curve assignments.
  targets::tar_target(
    name = "data_spd_radius_c14_filtered",
    command = filter_radiocarbon_data(
      data_source_c14 = data_spd_radius_c14,
      data_source_polygons = data_spd_radius_polygons,
      data_source_meta = data_spd_radius_metadata
    )
  ),
  # Why: Split the filtered input deterministically so each dataset becomes a
  #   restartable dynamic branch.
  targets::tar_target(
    name = "list_spd_radius_calculation_groups",
    command = prepare_spd_calculation_groups(
      data_source = data_spd_radius_c14_filtered,
      data_meta = data_spd_radius_metadata
    ),
    iteration = "list"
  ),
  # Why: Calculate both radii together within each restartable dataset branch.
  targets::tar_target(
    name = "data_spd_radius_dataset",
    command = compute_spd_by_nested_distances(
      data_source_c14 = list_spd_radius_calculation_groups,
      data_source_dist_vec = config_spd_radius[["radii_km"]],
      age_from = config_spd_radius[["age_from"]],
      age_to = config_spd_radius[["age_to"]],
      sel_smooth_size = config_spd_radius[["smoothing_years"]],
      min_n_dates = config_spd_radius[["minimum_dates"]],
      normalise_to_one = config_spd_radius[["normalise_to_one"]]
    ),
    pattern = map(list_spd_radius_calculation_groups),
    iteration = "list"
  ),
  # Why: Assemble dynamic branches only after every completed dataset result is
  #   available from the persistent target store.
  targets::tar_target(
    name = "data_spd_radius_wide",
    command = dplyr::bind_rows(data_spd_radius_dataset)
  ),
  # Why: Publish one explicit row for every dataset-radius combination,
  #   including unavailable all-zero series.
  targets::tar_target(
    name = "data_spd_by_radius",
    command = {
      res <- prepare_spd_radius_products(data_spd_radius_wide)
      validate_spd_radius_products(
        data_spd = res,
        expected_radii = config_spd_radius[["radii_km"]]
      )
      res
    }
  ),
  # Why: Apply the radius-selection policy once so all three public analysis
  #   datasets share one deterministic and tested contract.
  targets::tar_target(
    name = "list_spd_analysis_products",
    command = prepare_spd_analysis_products(data_spd_by_radius)
  ),
  # Why: Publish strict 250 km SPD data for radius-sensitive analyses without
  #   changing the canonical fallback predictor input.
  targets::tar_target(
    name = "data_spd_strict_250",
    command = list_spd_analysis_products[["data_spd_strict_250"]]
  ),
  # Why: Publish strict 500 km SPD data for radius-sensitive analyses without
  #   changing the canonical fallback predictor input.
  targets::tar_target(
    name = "data_spd_strict_500",
    command = list_spd_analysis_products[["data_spd_strict_500"]]
  ),
  # Why: Publish the default 250 km with 500 km fallback policy as a traceable
  #   product while retaining the historical artifact for current predictors.
  targets::tar_target(
    name = "data_spd_250_with_500_fallback",
    command =
      list_spd_analysis_products[[
        "data_spd_250_with_500_fallback"
      ]]
  ),
  # Why: Publish coverage overall and by the manuscript's geographic groups.
  targets::tar_target(
    name = "table_spd_radius_coverage",
    command = summarise_spd_radius_coverage(
      data_spd = data_spd_by_radius,
      data_meta = data_spd_radius_metadata
    )
  ),
  # Why: Preserve file, configuration, software, and graph fingerprints needed
  #   to trace each tested radius.
  targets::tar_target(
    name = "table_spd_radius_provenance",
    command = tibble::tibble(
      radius_km = config_spd_radius[["radii_km"]],
      c14_file = basename(file_spd_radius_c14),
      c14_file_md5 = unname(tools::md5sum(file_spd_radius_c14)),
      metadata_file = basename(file_spd_radius_metadata),
      metadata_file_md5 =
        unname(tools::md5sum(file_spd_radius_metadata)),
      configuration_hash = rlang::hash(config_spd_radius),
      products_hash = purrr::map_chr(
        config_spd_radius[["radii_km"]],
        ~ rlang::hash(
          dplyr::filter(
            data_spd_by_radius,
            .data[["radius_km"]] == .x
          )
        )
      ),
      r_version = R.version.string,
      rcarbon_version = as.character(
        utils::packageVersion("rcarbon")
      ),
      runner = runner_data_preparation
    )
  ),
  # Why: Load the historical 250 km artifact only for the required numerical
  #   regression audit.
  targets::tar_target(
    name = "data_spd_radius_reference_250",
    command = resolve_file_path(file_spd_radius_reference_250)
  ),
  # Why: Load the historical fallback artifact only for compatibility checks;
  #   prepared predictors continue to read this unchanged external file.
  targets::tar_target(
    name = "data_spd_radius_reference_fallback",
    command = resolve_file_path(file_spd_radius_reference_fallback)
  ),
  # Why: Confirm the recalculated 250 km curves retain the historical numerical
  #   contract without mutating canonical files.
  targets::tar_target(
    name = "table_spd_radius_reference_comparison",
    command = {
      res <- diagnose_spd_radius_reference(
        data_spd = data_spd_by_radius,
        data_reference = data_spd_radius_reference_250,
        radius_km = 250L,
        tolerance = sqrt(.Machine$double.eps)
      )

      if (
        any(
          res[["present_new"]] &
            res[["present_reference"]] &
            !res[["within_tolerance"]]
        )
      ) {
        cli::cli_abort(
          paste(
            "At least one overlapping recalculated 250 km SPD does not",
            "reproduce the historical artifact within tolerance."
          )
        )
      }

      res
    }
  ),
  # Why: Verify that the new fallback policy reproduces the historical product
  #   for every shared dataset while documenting cohort drift explicitly.
  targets::tar_target(
    name = "table_spd_fallback_reference_comparison",
    command = {
      res <- diagnose_spd_product_reference(
        data_spd = data_spd_250_with_500_fallback,
        data_reference = data_spd_radius_reference_fallback,
        tolerance = sqrt(.Machine$double.eps)
      )

      if (
        any(
          res[["present_new"]] &
            res[["present_reference"]] &
            !res[["within_tolerance"]]
        )
      ) {
        cli::cli_abort(
          paste(
            "The new SPD fallback product does not reproduce the",
            "historical artifact for every shared dataset."
          )
        )
      }

      res
    }
  )
)
