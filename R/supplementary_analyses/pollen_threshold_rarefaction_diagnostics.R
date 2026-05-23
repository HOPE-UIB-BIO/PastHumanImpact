#----------------------------------------------------------#
#
#                PastHumanImpact Diagnostics
#
#     Audit pollen thresholds and rarefaction behavior
#
#----------------------------------------------------------#

library(here)

source(
  here::here("R/00_Config_file.R")
)

#----------------------------------------------------------#
# 1. Configuration -----
#----------------------------------------------------------#

path_store_pollen <-
  file.path(
    data_storage_path,
    "Targets_data",
    "pipeline_pollen_data",
    "objects"
  )

path_output_tables <-
  here::here("Outputs", "Tables")

path_output_report <-
  here::here(
    "Manuscript",
    "_internal",
    "Pollen_threshold_rarefaction_diagnostic_report.md"
  )

if (
  isFALSE(dir.exists(path_output_tables))
) {
  dir.create(path_output_tables, recursive = TRUE)
}

#----------------------------------------------------------#
# 3. Load baseline target objects -----
#----------------------------------------------------------#

data_assembly_target <-
  get_file_from_path(
    file.path(
      path_store_pollen,
      "data_assembly"
    )
  )

data_assembly_filtered_target <-
  get_file_from_path(
    file.path(
      path_store_pollen,
      "data_assembly_filtered"
    )
  )

#----------------------------------------------------------#
# 4. Rarefaction audit -----
#----------------------------------------------------------#

diversity_formals <- formals(REcopol::diversity_estimate)

diversity_body <-
  deparse(body(REcopol::diversity_estimate))

diversity_uses_missing_sample_size <-
  any(grepl("if \\(missing\\(sample_size\\)\\)", diversity_body))

diversity_uses_min_rowsum <-
  any(grepl("min\\(\\)", diversity_body)) ||
    any(grepl("min\\(", diversity_body))

audit_rarefaction <-
  tibble::tibble(
    item = c(
      "filter_all_data_default_min_n_grains",
      "filter_all_data_default_target_n_grains",
      "get_diversity_passes_sample_size",
      "diversity_estimate_has_sample_size_argument",
      "diversity_estimate_uses_missing_sample_size_rule",
      "diversity_estimate_uses_min_rule_in_body"
    ),
    value = c(
      "25",
      "150",
      "FALSE",
      as.character("sample_size" %in% names(diversity_formals)),
      as.character(diversity_uses_missing_sample_size),
      as.character(diversity_uses_min_rowsum)
    )
  )

#----------------------------------------------------------#
# 5. Sample distributions -----
#----------------------------------------------------------#

data_samples_pre_filter <-
  extract_pollen_sample_counts(
    data_source = data_assembly_target,
    stage_label = "pre_filter"
  )

data_samples_post_filter <-
  extract_pollen_sample_counts(
    data_source = data_assembly_filtered_target,
    stage_label = "post_filter_baseline"
  )

data_samples_combined <-
  dplyr::bind_rows(
    data_samples_pre_filter,
    data_samples_post_filter
  )

data_record_region_climatezone_lookup <-
  data_samples_pre_filter |>
  dplyr::select(dataset_id, region, climatezone) |>
  dplyr::distinct()

table_bins_overall <-
  summarise_pollen_bin_distribution(
    data_source = data_samples_combined,
    group_cols = c("stage")
  )

table_bins_by_region <-
  summarise_pollen_bin_distribution(
    data_source = data_samples_combined,
    group_cols = c("stage", "region")
  )

table_bins_by_climatezone <-
  summarise_pollen_bin_distribution(
    data_source = data_samples_combined,
    group_cols = c("stage", "region", "climatezone")
  )

table_bins_by_time <-
  summarise_pollen_bin_distribution(
    data_source = data_samples_combined,
    group_cols = c("stage", "age_bin_start", "age_bin")
  )

table_bins_by_record <-
  summarise_pollen_bin_distribution(
    data_source = data_samples_combined,
    group_cols = c("stage", "region", "dataset_id")
  )

table_sample_counts_by_region <-
  data_samples_combined |>
  dplyr::group_by(stage, region) |>
  dplyr::summarise(
    n_samples = dplyr::n(),
    n_records = dplyr::n_distinct(dataset_id),
    median_rowsum = stats::median(rowsum, na.rm = TRUE),
    q90_rowsum = stats::quantile(rowsum, probs = 0.9, na.rm = TRUE),
    .groups = "drop"
  )

table_sample_counts_by_climatezone <-
  data_samples_combined |>
  dplyr::group_by(stage, region, climatezone) |>
  dplyr::summarise(
    n_samples = dplyr::n(),
    n_records = dplyr::n_distinct(dataset_id),
    median_rowsum = stats::median(rowsum, na.rm = TRUE),
    q90_rowsum = stats::quantile(rowsum, probs = 0.9, na.rm = TRUE),
    .groups = "drop"
  )

table_sample_counts_change_region <-
  table_sample_counts_by_region |>
  dplyr::select(stage, region, n_samples, n_records) |>
  tidyr::pivot_wider(
    names_from = stage,
    values_from = c(n_samples, n_records)
  ) |>
  dplyr::mutate(
    delta_samples = n_samples_post_filter_baseline - n_samples_pre_filter,
    delta_samples_prop = dplyr::if_else(
      n_samples_pre_filter > 0,
      delta_samples / n_samples_pre_filter,
      NA_real_
    ),
    delta_records = n_records_post_filter_baseline - n_records_pre_filter,
    delta_records_prop = dplyr::if_else(
      n_records_pre_filter > 0,
      delta_records / n_records_pre_filter,
      NA_real_
    )
  )

table_sample_counts_change_climatezone <-
  table_sample_counts_by_climatezone |>
  dplyr::select(stage, region, climatezone, n_samples, n_records) |>
  tidyr::pivot_wider(
    names_from = stage,
    values_from = c(n_samples, n_records)
  ) |>
  dplyr::mutate(
    delta_samples = n_samples_post_filter_baseline - n_samples_pre_filter,
    delta_samples_prop = dplyr::if_else(
      n_samples_pre_filter > 0,
      delta_samples / n_samples_pre_filter,
      NA_real_
    ),
    delta_records = n_records_post_filter_baseline - n_records_pre_filter,
    delta_records_prop = dplyr::if_else(
      n_records_pre_filter > 0,
      delta_records / n_records_pre_filter,
      NA_real_
    )
  )

table_high_count_context <-
  data_samples_combined |>
  dplyr::group_by(stage) |>
  dplyr::summarise(
    n_samples_total = dplyr::n(),
    n_samples_ge_150 = sum(rowsum >= 150, na.rm = TRUE),
    n_samples_ge_300 = sum(rowsum >= 300, na.rm = TRUE),
    n_samples_ge_500 = sum(rowsum >= 500, na.rm = TRUE),
    prop_samples_ge_150 = n_samples_ge_150 / n_samples_total,
    prop_samples_ge_300 = n_samples_ge_300 / n_samples_total,
    prop_samples_ge_500 = n_samples_ge_500 / n_samples_total,
    median_rowsum = stats::median(rowsum, na.rm = TRUE),
    q90_rowsum = stats::quantile(rowsum, probs = 0.9, na.rm = TRUE),
    .groups = "drop"
  )

table_sample_rowsums_by_climatezone <-
  data_samples_post_filter |>
  dplyr::select(
    dataset_id,
    sample_id,
    region,
    climatezone,
    rowsum
  )

#----------------------------------------------------------#
# 6. Threshold retention diagnostics -----
#----------------------------------------------------------#

threshold_scenarios <-
  tibble::tibble(
    scenario = c("baseline_25_150", "threshold_50_300", "threshold_84_500", "threshold_167_1000"),
    threshold = c(150, 300, 500, 1000)
  )

retention_records <-
  purrr::pmap(
    .l = list(threshold_scenarios$scenario, threshold_scenarios$threshold),
    .f = ~ {
      data_filtered <-
        filter_all_data(
          data_source = data_assembly_target,
          min_n_grains = (..2 / 6), # default as 25
          target_n_grains = ..2, # default as 150
          percentage_samples = 50,
          verbose = FALSE
        )

      data_filtered |>
        dplyr::mutate(
          scenario = ..1,
          threshold = ..2,
          n_samples_retained = n_sample_counts,
          dataset_id,
          .keep = "none"
        )
    }
  ) |>
  dplyr::bind_rows() |>
  dplyr::left_join(
    data_record_region_climatezone_lookup,
    by = "dataset_id"
  )

baseline_ids <-
  retention_records |>
  dplyr::filter(scenario == "baseline_25_150") |>
  dplyr::pull(dataset_id) |>
  unique()

n_baseline <- length(baseline_ids)

n_samples_baseline <-
  retention_records |>
  dplyr::filter(scenario == "baseline_25_150") |>
  dplyr::summarise(n_samples_baseline = sum(n_samples_retained, na.rm = TRUE)) |>
  dplyr::pull(n_samples_baseline)

table_retention_overall <-
  retention_records |>
  dplyr::group_by(scenario, threshold) |>
  dplyr::summarise(
    n_records_retained = dplyr::n_distinct(dataset_id),
    n_samples_retained = sum(n_samples_retained, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    n_records_baseline = n_baseline,
    n_samples_baseline = n_samples_baseline,
    n_records_lost = n_records_baseline - n_records_retained,
    n_samples_lost = n_samples_baseline - n_samples_retained,
    prop_retained = n_records_retained / n_records_baseline,
    prop_lost = n_records_lost / n_records_baseline,
    prop_samples_retained = n_samples_retained / n_samples_baseline,
    prop_samples_lost = n_samples_lost / n_samples_baseline
  )

baseline_by_region <-
  retention_records |>
  dplyr::filter(scenario == "baseline_25_150") |>
  dplyr::group_by(region) |>
  dplyr::summarise(
    n_region_baseline = dplyr::n_distinct(dataset_id),
    n_region_samples_baseline = sum(n_samples_retained, na.rm = TRUE),
    .groups = "drop"
  )

global_lost_ref <-
  table_retention_overall |>
  dplyr::select(scenario, prop_lost)

table_retention_by_region <-
  retention_records |>
  dplyr::group_by(scenario, threshold, region) |>
  dplyr::summarise(
    n_region_retained = dplyr::n_distinct(dataset_id),
    n_region_samples_retained = sum(n_samples_retained, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::left_join(
    baseline_by_region,
    by = "region"
  ) |>
  dplyr::mutate(
    n_region_lost = n_region_baseline - n_region_retained,
    n_region_samples_lost = n_region_samples_baseline - n_region_samples_retained,
    prop_region_retained = n_region_retained / n_region_baseline,
    prop_region_lost = n_region_lost / n_region_baseline,
    prop_region_samples_retained =
      dplyr::if_else(
        n_region_samples_baseline > 0,
        n_region_samples_retained / n_region_samples_baseline,
        NA_real_
      ),
    prop_region_samples_lost =
      dplyr::if_else(
        n_region_samples_baseline > 0,
        n_region_samples_lost / n_region_samples_baseline,
        NA_real_
      )
  ) |>
  dplyr::left_join(
    global_lost_ref,
    by = "scenario"
  ) |>
  dplyr::mutate(
    loss_delta_from_global = prop_region_lost - prop_lost
  ) |>
  dplyr::arrange(scenario, dplyr::desc(prop_region_lost), region)

baseline_by_region_climatezone <-
  retention_records |>
  dplyr::filter(scenario == "baseline_25_150") |>
  dplyr::group_by(region, climatezone) |>
  dplyr::summarise(
    n_region_climatezone_baseline = dplyr::n_distinct(dataset_id),
    n_region_climatezone_samples_baseline = sum(n_samples_retained, na.rm = TRUE),
    .groups = "drop"
  )

table_retention_by_region_climatezone <-
  retention_records |>
  dplyr::group_by(scenario, threshold, region, climatezone) |>
  dplyr::summarise(
    n_region_climatezone_retained = dplyr::n_distinct(dataset_id),
    n_region_climatezone_samples_retained = sum(n_samples_retained, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::left_join(
    baseline_by_region_climatezone,
    by = c("region", "climatezone")
  ) |>
  dplyr::mutate(
    n_region_climatezone_lost =
      n_region_climatezone_baseline - n_region_climatezone_retained,
    n_region_climatezone_samples_lost =
      n_region_climatezone_samples_baseline - n_region_climatezone_samples_retained,
    prop_region_climatezone_retained =
      dplyr::if_else(
        n_region_climatezone_baseline > 0,
        n_region_climatezone_retained / n_region_climatezone_baseline,
        NA_real_
      ),
    prop_region_climatezone_lost =
      dplyr::if_else(
        n_region_climatezone_baseline > 0,
        n_region_climatezone_lost / n_region_climatezone_baseline,
        NA_real_
      ),
    prop_region_climatezone_samples_retained =
      dplyr::if_else(
        n_region_climatezone_samples_baseline > 0,
        n_region_climatezone_samples_retained / n_region_climatezone_samples_baseline,
        NA_real_
      ),
    prop_region_climatezone_samples_lost =
      dplyr::if_else(
        n_region_climatezone_samples_baseline > 0,
        n_region_climatezone_samples_lost / n_region_climatezone_samples_baseline,
        NA_real_
      )
  ) |>
  dplyr::arrange(
    scenario,
    region,
    dplyr::desc(prop_region_climatezone_lost),
    climatezone
  )

#----------------------------------------------------------#
# 7. Reconciliation checks -----
#----------------------------------------------------------#

n_samples_pre <- nrow(data_samples_pre_filter)
n_samples_post <- nrow(data_samples_post_filter)

n_records_target_filtered <-
  data_assembly_filtered_target |>
  dplyr::pull(dataset_id) |>
  unique() |>
  length()

n_records_baseline_runtime <-
  table_retention_overall |>
  dplyr::filter(scenario == "baseline_25_150") |>
  dplyr::pull(n_records_retained)

#----------------------------------------------------------#
# 8. Save outputs -----
#----------------------------------------------------------#

readr::write_csv(
  table_sample_rowsums_by_climatezone,
  file.path(path_output_tables, "pollen_sample_rowsums_by_climatezone.csv")
)

readr::write_csv(
  table_retention_overall,
  file.path(path_output_tables, "pollen_threshold_retention_overall.csv")
)

readr::write_csv(
  table_retention_by_region,
  file.path(path_output_tables, "pollen_threshold_retention_by_region.csv")
)

readr::write_csv(
  table_retention_by_region_climatezone,
  file.path(path_output_tables, "pollen_threshold_retention_by_region_climatezone.csv")
)
