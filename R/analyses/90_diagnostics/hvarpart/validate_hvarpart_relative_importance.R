#----------------------------------------------------------#
#
#                     GlobalHumanImpact
#
#           Validate HVarPart relative importance
#
#                         2026
#
#----------------------------------------------------------#

#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#
library(here)
source(here::here("R/00_Config_file.R"))

table_dir <-
  here::here("Outputs", "Tables", "Diagnostics", "HVarPart")

spatial_data_dir <-
  here::here("Outputs", "Tables", "H1", "Spatial", "SPD")

temporal_data_dir <-
  here::here("Outputs", "Tables", "H1", "Temporal", "HVarPart")

interrelationships_data_dir <-
  here::here("Outputs", "Tables", "H2", "Interrelationships")
spatial_figure_dir <-
  here::here("Outputs/Figures/H1/Spatial/SPD")

temporal_figure_dir <-
  here::here("Outputs/Figures/H1/Temporal/HVarPart")

interrelationships_figure_dir <-
  here::here("Outputs/Figures/H2/Interrelationships")

diagnostic_figure_dir <- here::here(
  "Outputs/Figures/Diagnostics/HVarPart"
)

#----------------------------------------------------------#
# 1. Load exported data -----
#----------------------------------------------------------#
data_components <- readr::read_csv(
  file.path(table_dir, "hvarpart__components.csv"),
  show_col_types = FALSE,
  guess_max = Inf
)
data_components_default <- suppressWarnings(
  readr::read_csv(
    file.path(table_dir, "hvarpart__components.csv"),
    show_col_types = FALSE
  )
)
data_audit <- readr::read_csv(
  file.path(table_dir, "hvarpart__model_audit.csv"),
  show_col_types = FALSE,
  guess_max = Inf
)
data_profiles <- readr::read_csv(
  file.path(table_dir, "hvarpart__profile_comparison.csv"),
  show_col_types = FALSE,
  guess_max = Inf
)
data_untruncated_spatial_contributions <- readr::read_csv(
  file.path(
    spatial_data_dir,
    "spd__human_climate_balance__untruncated_dataset_values.csv"
  ),
  show_col_types = FALSE,
  guess_max = Inf
)
data_time_space_controlled_balance <- readr::read_csv(
  file.path(
    spatial_data_dir,
    stringr::str_c(
      "spd__human_climate_balance__dataset_values__",
      "time_and_space_control.csv"
    )
  ),
  show_col_types = FALSE,
  guess_max = Inf
)
data_time_space_controlled_climate_zone <- readr::read_csv(
  file.path(
    spatial_data_dir,
    stringr::str_c(
      "spd__human_climate_balance__climate_zone_values__",
      "time_and_space_control.csv"
    )
  ),
  show_col_types = FALSE,
  guess_max = Inf
)
data_time_space_controlled_region <- readr::read_csv(
  file.path(
    spatial_data_dir,
    "spd__human_climate_balance__region_values__human_climate_only.csv"
  ),
  show_col_types = FALSE,
  guess_max = Inf
)
data_temporal_balance <- readr::read_csv(
  file.path(
    temporal_data_dir,
    "spd_events__human_climate_balance__human_climate_only.csv"
  ),
  show_col_types = FALSE,
  guess_max = Inf
)
data_interrelationships <- readr::read_csv(
  file.path(
    interrelationships_data_dir,
    "predictors__interrelationships__importance_values.csv"
  ),
  show_col_types = FALSE,
  guess_max = Inf
)

#----------------------------------------------------------#
# 2. Reconcile formulas and exclusions -----
#----------------------------------------------------------#
duplicate_keys <-
  data_components |>
  dplyr::count(.data[["model_id"]], .data[["predictor"]]) |>
  dplyr::filter(.data[["n"]] != 1L)

data_signed_overall <-
  data_profiles |>
  dplyr::filter(
    .data[["aggregation_level"]] == "analysis",
    .data[["profile"]] == "signed"
  )

formula_ok <- all(
  abs(
    data_signed_overall$pooled_allocation -
      data_signed_overall$individual_sum / data_signed_overall$total_sum
  ) < 1e-12
)
paired_sum_ok <-
  data_signed_overall |>
  dplyr::group_by(.data[["analysis"]]) |>
  dplyr::summarise(
    allocation_sum = sum(.data[["pooled_allocation"]]),
    .groups = "drop"
  ) |>
  dplyr::summarise(ok = all(abs(.data[["allocation_sum"]] - 1) < 5e-4)) |>
  dplyr::pull(.data[["ok"]])

audit_overall <-
  data_audit |>
  dplyr::filter(.data[["aggregation_level"]] == "analysis")
audit_ok <- all(
  audit_overall$n_models ==
    audit_overall$n_eligible + audit_overall$n_excluded
) && all(
  audit_overall$n_excluded ==
    audit_overall$n_missing_result +
      audit_overall$n_missing_predictor +
      audit_overall$n_non_finite_total +
      audit_overall$n_non_positive_total +
      audit_overall$n_non_finite_individual
)

plot_untruncated_spatial_contributions_ok <- all(
  abs(
    data_untruncated_spatial_contributions$signed_allocation -
      data_untruncated_spatial_contributions$individual /
        data_untruncated_spatial_contributions$total_adjusted_r_squared
  ) < 1e-12
)
spatial_zero_truncated_values_ok <- all(
  abs(
    data_untruncated_spatial_contributions$zero_truncated_allocation -
      data_untruncated_spatial_contributions$zero_truncated_individual /
        data_untruncated_spatial_contributions$zero_truncated_total
  ) < 1e-12
)
spatial_human_only_ok <- identical(
  unique(data_untruncated_spatial_contributions$predictor),
  "human"
)
plot_time_space_controlled_balance_formula_ok <- all(
  abs(
    data_time_space_controlled_balance$importance_balance -
      (
        data_time_space_controlled_balance$human -
          data_time_space_controlled_balance$climate
      )
  ) < 1e-12
)
plot_time_space_controlled_balance_range_ok <- all(
  data_time_space_controlled_balance$importance_balance >= -1 &
    data_time_space_controlled_balance$importance_balance <= 1
)
plot_time_space_controlled_balance_pooled_ok <- all(
  abs(
    data_time_space_controlled_climate_zone$importance_balance -
      (
        data_time_space_controlled_climate_zone$human -
          data_time_space_controlled_climate_zone$climate
      )
  ) < 1e-12
) && all(
  abs(
    data_time_space_controlled_region$importance_balance -
      (
        data_time_space_controlled_region$human -
          data_time_space_controlled_region$climate
      )
  ) < 1e-12
)
temporal_balance_formula_ok <- all(
  abs(
    data_temporal_balance$importance_balance -
      (
        data_temporal_balance$human -
          data_temporal_balance$climate
      )
  ) < 1e-12
)
temporal_balance_range_ok <- all(
  data_temporal_balance$importance_balance >= -1 &
    data_temporal_balance$importance_balance <= 1
)
temporal_balance_age_domain_ok <- all(
  dplyr::between(data_temporal_balance$age, 0, 8500)
) && !any(
  data_temporal_balance$analysis == "temporal_spd" &
    data_temporal_balance$age < 2000
)
data_interrelationships_balance <-
  data_interrelationships |>
  dplyr::filter(.data[["profile"]] == "balance")
interrelationships_balance_formula_ok <- all(
  abs(
    data_interrelationships_balance$importance_balance -
      (
        data_interrelationships_balance$human -
          data_interrelationships_balance$climate
      )
  ) < 1e-12
)
interrelationships_balance_range_ok <- all(
  data_interrelationships_balance$importance_balance >= -1 &
    data_interrelationships_balance$importance_balance <= 1
)

conclusion_ok <-
  data_profiles |>
  dplyr::filter(.data[["aggregation_level"]] == "analysis") |>
  dplyr::select(
    dplyr::all_of(
      c("analysis", "profile", "predictor", "pooled_allocation")
    )
  ) |>
  tidyr::pivot_wider(
    names_from = "predictor",
    values_from = "pooled_allocation"
  ) |>
  dplyr::summarise(ok = all(.data[["climate"]] > .data[["human"]])) |>
  dplyr::pull(.data[["ok"]])

profile_model_distribution_ok <-
  data_profiles |>
  dplyr::filter(.data[["aggregation_level"]] == "model") |>
  dplyr::summarise(
    ok = dplyr::n() > 0L &&
      all(is.finite(.data[["pooled_allocation"]])) &&
      all(.data[["n_models"]] == 1L)
  ) |>
  dplyr::pull(.data[["ok"]])

#----------------------------------------------------------#
# 3. Reconcile main and supplementary artifacts -----
#----------------------------------------------------------#
artifact_paths <- c(
  file.path(table_dir, "hvarpart__components.csv"),
  file.path(table_dir, "hvarpart__model_audit.csv"),
  file.path(table_dir, "hvarpart__profile_comparison.csv"),
  file.path(
    spatial_data_dir,
    stringr::str_c(
      "spd__human_climate_time__component_profiles__",
      "dataset_values__time_control.csv"
    )
  ),
  file.path(
    spatial_data_dir,
    stringr::str_c(
      "spd__human_climate_time__component_profiles__",
      "climate_zone_values__time_control.csv"
    )
  ),
  file.path(
    spatial_data_dir,
    stringr::str_c(
      "spd__human_climate_time__component_profiles__",
      "region_values__time_control.csv"
    )
  ),
  file.path(
    spatial_data_dir,
    stringr::str_c(
      "spd__human_climate_balance__dataset_values__",
      "time_and_space_control.csv"
    )
  ),
  file.path(
    spatial_data_dir,
    stringr::str_c(
      "spd__human_climate_balance__climate_zone_values__",
      "time_and_space_control.csv"
    )
  ),
  file.path(
    spatial_data_dir,
    "spd__human_climate_balance__region_values__human_climate_only.csv"
  ),
  file.path(
    temporal_data_dir,
    "spd_events__human_climate_balance__human_climate_only.csv"
  ),
  file.path(
    interrelationships_data_dir,
    "predictors__interrelationships__importance_values.csv"
  ),
  file.path(
    spatial_figure_dir,
    stringr::str_c(
      "spd",
      "human_climate_balance",
      "zero_truncated_hierarchical_composition",
      "time_and_space_control.png",
      sep = "__"
    )
  ),
  file.path(
    spatial_figure_dir,
    stringr::str_c(
      "spd",
      "human_climate_balance",
      "zero_truncated_hierarchical_composition",
      "time_and_space_control.pdf",
      sep = "__"
    )
  ),
  file.path(
    temporal_figure_dir,
    stringr::str_c(
      "spd_events__human_climate_space__",
      "zero_truncated_hierarchical_composition__space_control.png"
    )
  ),
  file.path(
    temporal_figure_dir,
    stringr::str_c(
      "spd_events__human_climate_space__",
      "zero_truncated_hierarchical_composition__space_control.pdf"
    )
  ),
  file.path(
    interrelationships_figure_dir,
    "predictor_interrelationships.png"
  ),
  file.path(
    interrelationships_figure_dir,
    "predictor_interrelationships.pdf"
  ),
  file.path(
    spatial_figure_dir,
    stringr::str_c(
      "spd",
      "human_climate_balance",
      "untruncated_hierarchical_contribution_difference",
      "time_and_space_control.png",
      sep = "__"
    )
  ),
  file.path(
    spatial_figure_dir,
    stringr::str_c(
      "spd",
      "human_climate_balance",
      "untruncated_hierarchical_contribution_difference",
      "time_and_space_control.pdf",
      sep = "__"
    )
  ),
  file.path(
    temporal_figure_dir,
    stringr::str_c(
      "spd_events__human_climate__",
      "untruncated_hierarchical_contributions__human_climate_only.png"
    )
  ),
  file.path(
    temporal_figure_dir,
    stringr::str_c(
      "spd_events__human_climate__",
      "untruncated_hierarchical_contributions__human_climate_only.pdf"
    )
  ),
  file.path(
    interrelationships_figure_dir,
    stringr::str_c(
      "predictor_interrelationships_",
      "untruncated_hierarchical_contributions.png"
    )
  ),
  file.path(
    interrelationships_figure_dir,
    stringr::str_c(
      "predictor_interrelationships_",
      "untruncated_hierarchical_contributions.pdf"
    )
  ),
  file.path(
    diagnostic_figure_dir,
    "hvarpart__hierarchical_profile_comparison.png"
  ),
  file.path(
    diagnostic_figure_dir,
    "hvarpart__hierarchical_profile_comparison.pdf"
  )
)

validation <- tibble::tibble(
  check = c(
    "unique_model_predictor_keys",
    "components_age_parses_with_default_reader",
    "signed_direct_formula",
    "paired_signed_allocations_sum_to_one",
    "audit_exclusions_reconcile",
    "plot_untruncated_spatial_contributions_values_match_components",
    "spatial_zero_truncated_values_match_components",
    "plot_untruncated_spatial_contributions_supplement_displays_human_only",
    "plot_time_space_controlled_balance_matches_human_minus_climate",
    "plot_time_space_controlled_balance_is_bounded",
    "plot_time_space_controlled_balance_pooled_values_reconcile",
    "temporal_balance_matches_human_minus_climate",
    "temporal_balance_is_bounded",
    "temporal_spd_is_restricted_to_2_8.5_ka",
    "interrelationships_balance_matches_human_minus_climate",
    "interrelationships_balance_is_bounded",
    "climate_larger_under_all_profiles",
    "profile_comparison_contains_model_distributions",
    "main_and_signed_supplementary_artifacts_exist"
  ),
  passed = c(
    nrow(duplicate_keys) == 0L,
    is.numeric(data_components_default[["age"]]) &&
      !any(
        is.na(data_components_default[["age"]][
          data_components_default[["analysis"]] == "temporal_spd"
        ])
      ),
    formula_ok,
    paired_sum_ok,
    audit_ok,
    plot_untruncated_spatial_contributions_ok,
    spatial_zero_truncated_values_ok,
    spatial_human_only_ok,
    plot_time_space_controlled_balance_formula_ok,
    plot_time_space_controlled_balance_range_ok,
    plot_time_space_controlled_balance_pooled_ok,
    temporal_balance_formula_ok,
    temporal_balance_range_ok,
    temporal_balance_age_domain_ok,
    interrelationships_balance_formula_ok,
    interrelationships_balance_range_ok,
    conclusion_ok,
    profile_model_distribution_ok,
    all(file.exists(artifact_paths))
  )
)

readr::write_csv(
  validation,
  file.path(table_dir, "hvarpart__validation_provenance.csv")
)

assertthat::assert_that(
  all(validation$passed),
  msg = "At least one HVarPart reconciliation check failed."
)
