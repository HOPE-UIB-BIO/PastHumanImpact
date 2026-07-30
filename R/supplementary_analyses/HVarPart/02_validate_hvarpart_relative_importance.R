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

table_dir <- here::here("Outputs/Tables/HVarPart")
general_table_dir <- here::here("Outputs/Tables")
figure_dir <- here::here("Outputs/Figures")
supplementary_figure_dir <- here::here(
  "Outputs/Figures/Extended_data_figures/HVarPart"
)

#----------------------------------------------------------#
# 1. Load exported data -----
#----------------------------------------------------------#
data_components <- readr::read_csv(
  file.path(table_dir, "hvarpart_components.csv"),
  show_col_types = FALSE,
  guess_max = Inf
)
data_components_default <- suppressWarnings(
  readr::read_csv(
    file.path(table_dir, "hvarpart_components.csv"),
    show_col_types = FALSE
  )
)
data_audit <- readr::read_csv(
  file.path(table_dir, "hvarpart_model_audit.csv"),
  show_col_types = FALSE,
  guess_max = Inf
)
data_profiles <- readr::read_csv(
  file.path(table_dir, "hvarpart_profile_comparison.csv"),
  show_col_types = FALSE,
  guess_max = Inf
)
data_figure2 <- readr::read_csv(
  file.path(table_dir, "figure2_record_values.csv"),
  show_col_types = FALSE,
  guess_max = Inf
)
data_figure2_balance <- readr::read_csv(
  file.path(table_dir, "figure2_balance_record_values.csv"),
  show_col_types = FALSE,
  guess_max = Inf
)
data_figure2_balance_climatezone <- readr::read_csv(
  file.path(table_dir, "figure2_balance_climatezone_values.csv"),
  show_col_types = FALSE,
  guess_max = Inf
)
data_figure2_balance_region <- readr::read_csv(
  file.path(table_dir, "figure2_balance_region_values.csv"),
  show_col_types = FALSE,
  guess_max = Inf
)
data_temporal_balance <- readr::read_csv(
  file.path(general_table_dir, "summary_temporal_balance.csv"),
  show_col_types = FALSE,
  guess_max = Inf
)
data_figure4 <- readr::read_csv(
  file.path(table_dir, "figure4_importance_values.csv"),
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

figure2_signed_ok <- all(
  abs(
    data_figure2$signed_allocation -
      data_figure2$individual / data_figure2$total_adjusted_r_squared
  ) < 1e-12
)
figure2_zero_ok <- all(
  abs(
    data_figure2$zero_truncated_allocation -
      data_figure2$zero_truncated_individual /
        data_figure2$zero_truncated_total
  ) < 1e-12
)
figure2_human_only_ok <- identical(
  unique(data_figure2$predictor),
  "human"
)
figure2_balance_formula_ok <- all(
  abs(
    data_figure2_balance$importance_balance -
      (
        data_figure2_balance$human -
          data_figure2_balance$climate
      )
  ) < 1e-12
)
figure2_balance_range_ok <- all(
  data_figure2_balance$importance_balance >= -1 &
    data_figure2_balance$importance_balance <= 1
)
figure2_balance_pooled_ok <- all(
  abs(
    data_figure2_balance_climatezone$importance_balance -
      (
        data_figure2_balance_climatezone$human -
          data_figure2_balance_climatezone$climate
      )
  ) < 1e-12
) && all(
  abs(
    data_figure2_balance_region$importance_balance -
      (
        data_figure2_balance_region$human -
          data_figure2_balance_region$climate
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
data_figure4_balance <-
  data_figure4 |>
  dplyr::filter(.data[["profile"]] == "balance")
figure4_balance_formula_ok <- all(
  abs(
    data_figure4_balance$importance_balance -
      (
        data_figure4_balance$human -
          data_figure4_balance$climate
      )
  ) < 1e-12
)
figure4_balance_range_ok <- all(
  data_figure4_balance$importance_balance >= -1 &
    data_figure4_balance$importance_balance <= 1
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
  file.path(table_dir, "hvarpart_components.csv"),
  file.path(table_dir, "hvarpart_model_audit.csv"),
  file.path(table_dir, "hvarpart_profile_comparison.csv"),
  file.path(table_dir, "figure2_record_values.csv"),
  file.path(table_dir, "figure2_pooled_values.csv"),
  file.path(table_dir, "figure2_region_values.csv"),
  file.path(table_dir, "figure2_balance_record_values.csv"),
  file.path(table_dir, "figure2_balance_climatezone_values.csv"),
  file.path(table_dir, "figure2_balance_region_values.csv"),
  file.path(general_table_dir, "summary_temporal_balance.csv"),
  file.path(table_dir, "figure4_importance_values.csv"),
  file.path(figure_dir, "Figure2_h1_spatial.png"),
  file.path(figure_dir, "Figure2_h1_spatial.pdf"),
  file.path(figure_dir, "Figure3_h1_temporal.png"),
  file.path(figure_dir, "Figure3_h1_temporal.pdf"),
  file.path(figure_dir, "Figure4_h2.png"),
  file.path(figure_dir, "Figure4_h2.pdf"),
  file.path(
    supplementary_figure_dir,
    "Figure2_h1_spatial_signed_full_range.png"
  ),
  file.path(
    supplementary_figure_dir,
    "Figure2_h1_spatial_signed_full_range.pdf"
  ),
  file.path(
    supplementary_figure_dir,
    "Figure3_h1_temporal_signed_full_range.png"
  ),
  file.path(
    supplementary_figure_dir,
    "Figure3_h1_temporal_signed_full_range.pdf"
  ),
  file.path(
    supplementary_figure_dir,
    "Figure4_h2_signed_full_range.png"
  ),
  file.path(
    supplementary_figure_dir,
    "Figure4_h2_signed_full_range.pdf"
  ),
  file.path(
    supplementary_figure_dir,
    "hvarpart_profile_comparison.png"
  ),
  file.path(
    supplementary_figure_dir,
    "hvarpart_profile_comparison.pdf"
  )
)

validation <- tibble::tibble(
  check = c(
    "unique_model_predictor_keys",
    "components_age_parses_with_default_reader",
    "signed_direct_formula",
    "paired_signed_allocations_sum_to_one",
    "audit_exclusions_reconcile",
    "figure2_signed_values_match_components",
    "figure2_zero_truncated_values_match_components",
    "figure2_signed_supplement_displays_human_only",
    "figure2_balance_matches_human_minus_climate",
    "figure2_balance_is_bounded",
    "figure2_balance_pooled_values_reconcile",
    "figure3_balance_matches_human_minus_climate",
    "figure3_balance_is_bounded",
    "figure3_spd_is_restricted_to_2_8.5_ka",
    "figure4_balance_matches_human_minus_climate",
    "figure4_balance_is_bounded",
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
    figure2_signed_ok,
    figure2_zero_ok,
    figure2_human_only_ok,
    figure2_balance_formula_ok,
    figure2_balance_range_ok,
    figure2_balance_pooled_ok,
    temporal_balance_formula_ok,
    temporal_balance_range_ok,
    temporal_balance_age_domain_ok,
    figure4_balance_formula_ok,
    figure4_balance_range_ok,
    conclusion_ok,
    profile_model_distribution_ok,
    all(file.exists(artifact_paths))
  )
)

readr::write_csv(
  validation,
  file.path(table_dir, "hvarpart_validation_provenance.csv")
)

assertthat::assert_that(
  all(validation$passed),
  msg = "At least one HVarPart reconciliation check failed."
)
