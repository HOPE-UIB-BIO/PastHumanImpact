library(here)
source(here::here("R/00_Config_file.R"))

table_dir <- here::here("Outputs/Tables/HVarPart")
figure_dir <- here::here("Outputs/Figures/Extended_data_figures/HVarPart")

data_components <- readr::read_csv(
  file.path(table_dir, "hvarpart_components.csv"),
  show_col_types = FALSE,
  guess_max = Inf
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
  dplyr::pull("ok")

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

figure2_ok <- all(
  abs(
    data_figure2$signed_allocation -
      data_figure2$individual / data_figure2$total_adjusted_r_squared
  ) < 1e-12
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
  dplyr::pull("ok")

artifact_paths <- c(
  file.path(table_dir, "hvarpart_components.csv"),
  file.path(table_dir, "hvarpart_model_audit.csv"),
  file.path(table_dir, "hvarpart_profile_comparison.csv"),
  file.path(table_dir, "figure2_record_values.csv"),
  file.path(table_dir, "figure2_pooled_values.csv"),
  file.path(table_dir, "figure2_display_tail_counts.csv"),
  file.path(figure_dir, "Figure2_h1_spatial_full_range.png"),
  file.path(figure_dir, "Figure2_h1_spatial_full_range.pdf"),
  file.path(figure_dir, "hvarpart_profile_comparison.png"),
  file.path(figure_dir, "hvarpart_profile_comparison.pdf")
)

validation <- tibble::tibble(
  check = c(
    "unique_model_predictor_keys",
    "signed_direct_formula",
    "paired_signed_allocations_sum_to_one",
    "audit_exclusions_reconcile",
    "figure2_values_match_components",
    "climate_larger_under_all_profiles",
    "required_artifacts_exist"
  ),
  passed = c(
    nrow(duplicate_keys) == 0L,
    formula_ok,
    paired_sum_ok,
    audit_ok,
    figure2_ok,
    conclusion_ok,
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
