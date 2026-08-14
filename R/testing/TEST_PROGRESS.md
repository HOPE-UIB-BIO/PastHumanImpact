# Test Progress

This file tracks the project-wide test rollout for the `testthat` suite under `R/tests/testthat`.

## Current Status

- Test runner: complete
- Smoke test: complete
- Contract-first rollout: complete
- Contract coverage gate: complete (all checked functions have roxygen, argument assertions, and tests)
- Latest full-suite validation: `[ FAIL 0 | WARN 1 | SKIP 1 | PASS 759 ]`, `EXIT_CODE=0`

### Active Batches

- none (rollout complete)

### Fresh Starting Points

- none currently listed

### Completed In This Iteration

- `compute_hvarpart_importance`: nested HVarPart results are validated, failed fits are ignored, duplicate estimates are consolidated, and explained-variation shares are normalised for plotting; focused tests cover each case.
- `plot_hvarpart_importance`: human and climate importance estimates are displayed independently around zero alongside total explained variation; focused plotting tests added.
- `plot_hvarpart_dataset_temporal_example`: current raw, interpolated, and modelled core trajectories are combined with HVarPart results; focused composition tests added.
- `plot_predictor_temporal_trends`: predictor-wide temporal panels now use a reusable plotting contract and semantic supplementary outputs; focused plotting tests added.
- `load_chelsa_archive`: input contract hardened for required download metadata columns and control argument types; new per-function tests added for invalid-input coverage.
- `load_climate_data`: input contract hardened for selected-variable vectors and required coordinate columns in `xy`; new per-function tests added for invalid-input coverage.
- `aggregate_events_spd`: contract expanded (including roxygen params/return) with required input-shape validation; new per-function tests added for invalid-input coverage.
- `fit_brms_model`: roxygen contract added to document modelling intent, arguments, and failure return behavior.
- `aggregate_density_pap`: input contract hardened for required source/meta/dummy-time columns and logical controls; tests expanded with explicit invalid-input coverage.
- `prepare_m2_data`: roxygen contract added, input contract hardened for source/meta shape and nested payloads, and a new per-function test file added with happy-path and invalid-input coverage.
- `fit_pca`: input contract hardened for source table shape and `scale` flag, roxygen contract expanded, tests expanded with explicit invalid-input coverage.
- `fit_pcoa`: input contract hardened for matrix shape and NA handling, roxygen contract expanded, tests expanded with explicit invalid-input coverage.
- `fit_dbrda`: roxygen contract added and input contract hardened for matrix/predictor shape on non-NULL execution path while preserving NULL short-circuit behavior; tests expanded with invalid-input coverage.
- `compute_m2_time`: input contract hardened for matrix and square-shape requirements; tests expanded with explicit invalid-input coverage.
- `prepare_m2_time_data`: input contract hardened for atomic-vector requirements; tests expanded with explicit invalid-input coverage.
- `compute_procrustes_m2`: input contract hardened for non-empty named list model inputs; tests expanded with explicit invalid-input coverage.
- `compute_diversity`: input contract hardened for required source columns, nested count tables, and control arguments; tests expanded with invalid-input coverage.
- `compute_dcca`: input contract hardened for required source columns and nested data payloads; tests expanded with invalid-input coverage.
- `prepare_data_cp`: input contract hardened for required source columns and nested payload shapes across all input tables; tests expanded with invalid-input coverage.
- `prepare_diversity_dcca_model_data`: roxygen contract added, explicit input validation added for source tables and nested payload columns, tests expanded with invalid-input coverage.
- `prepare_interpolated_model_data`: roxygen contract added, explicit input validation added for source shape, nested payloads, and selector arguments, tests expanded with invalid-input coverage.
- `predict_brms_model`: roxygen contract added, explicit model-object validation added, tests expanded with invalid-input coverage.
- `prepare_event_model_data`: roxygen contract added, explicit input validation added for source table and nested `events_updated` payloads, tests expanded with invalid-input coverage.
- `prepare_spd_model_data`: roxygen contract added, explicit input validation added for source table and nested `spd` payloads, tests expanded with invalid-input coverage.
- `prepare_roc_model_data`: roxygen contract added, explicit input validation added for source table and nested `PAP_roc` payloads, tests expanded with invalid-input coverage.
- `resolve_model_rerun_flag`: roxygen contract added, explicit input validation added for required config columns and selector args, tests expanded with invalid-input coverage.
- `predict_general_trends`: roxygen contract added, explicit input validation added for required columns and storage-path availability, tests expanded with invalid-input coverage.
- `build_hgam_formula`: roxygen contract added and tests expanded with invalid-input coverage for grouping-count validation.
- `build_chelsa_trace21k_catalog`: input contract hardened with explicit argument validation and explicit namespace usage for string and case helpers; tests expanded with invalid-input coverage.
- `prepare_climate_interpolation_data`: roxygen contract added, explicit input validation added for source shape and nested climate tables, deprecated selection warning removed, tests expanded with invalid-input coverage.
- HVarPart importance now uses the canonical raw extractor and explicit signed/sensitivity summaries; the overlapping legacy summary helper was removed.
- `aggregate_events`: roxygen contract expanded, explicit input validation added for variadic data-frame inputs, tests expanded with invalid-input coverage.
- `prepare_pollen_data`: input contract hardened for source table and requested variable selection, tests expanded with invalid-input coverage.
- `run_directory_setup`: roxygen contract added, explicit path-type validation added, tests aligned to explicit invalid-input failure behavior.
- `classify_indicator_events`: roxygen contract expanded, explicit input validation added for required indicator/pollen/meta columns and selector arguments, tests expanded with invalid-input coverage.
- `classify_index_events`: roxygen contract expanded, explicit input validation added for required index/pollen/meta columns and selector arguments, message output gated behind `verbose`, tests expanded with invalid-input coverage.
- `classify_binary_events`: tests expanded with additional invalid-input coverage for missing pollen columns and malformed nested `events_age` tables.
- `prepare_h2_hvarpart_data`: roxygen contract added, explicit input validation for required predictor and m2 columns added, tests expanded with invalid-input coverage.
- `prepare_filtered_hvarpart_data`: roxygen contract added, explicit input validation for inputs and age bounds added, `remove_private = FALSE` branch fixed to use `data_meta`, tests updated and expanded with invalid-input coverage.
- `prepare_hvarpart_timebin_data`: roxygen contract added, explicit input validation for required source/meta columns added, tests expanded with invalid-input coverage.
- `compute_dbrda_scores`: roxygen contract added, explicit validation added for model-like non-atomic input (or `NULL`), tests expanded with invalid-input coverage.
- `prepare_climate_ages`: contract hardened with roxygen and explicit input validation for required source and translation columns; tests expanded with invalid-input coverage.
- `prepare_age_factor`: roxygen contract added, explicit input validation for required numeric `age` added, tests expanded with invalid-input coverage.
- `prepare_climatezone_factor`: roxygen contract added, explicit input validation for required `climatezone` and config table columns added, tests expanded with invalid-input coverage.
- `prepare_predictor_factor`: roxygen contract added, explicit input validation for required `predictor` column added, tests expanded with invalid-input coverage.
- `prepare_region_factor`: roxygen contract added, explicit input validation for required `region` column added, tests expanded with invalid-input coverage.
- `resolve_climatezone_label`: roxygen contract added, explicit character-input validation added, tests expanded with invalid-input coverage.
- `aggregate_event_sources`: roxygen contract tightened, explicit input validation added, tests expanded with invalid-input coverage.
- `classify_events_by_logical_rules`: roxygen contract tightened, explicit input validation and unsupported-region failures added, message output gated behind `verbose`, tests expanded with invalid-input coverage.
- `filter_event_types`: roxygen contract tightened, explicit input validation and unsupported-region failures added, data-frame event payload handling fixed, tests expanded with invalid-input coverage.
- `classify_binary_events`: roxygen contract tightened, explicit input validation added, message output gated behind `verbose`, tests aligned to function contract and validation behavior.
- `resolve_file_path`: roxygen contract tightened, explicit path validation added, tests expanded with invalid-input coverage.
- `summarise_data_properties`: roxygen contract added, explicit input validation added for required columns, data source shape, and `used_rescale`, tests expanded with invalid-input coverage.
- `prepare_combined_data`: roxygen contract added, explicit required-column validation added, tests expanded with invalid-input coverage.
- `prepare_metadata`: explicit input validation added for assembly shape and requested variables, tests expanded with invalid-input coverage.
- `validate_storage_folders`: roxygen contract added, explicit path validation added, tests expanded with invalid-input coverage.
- `prepare_predictor_data`: roxygen contract added, explicit input validation added, tests expanded with invalid-input coverage.
- `compute_roc`: contract and validation hardened (including verbose and injectable estimation/detection hooks for unit tests), and a new per-function test file added with happy-path and invalid-input coverage.
- `fit_hvarpart_models`: input contract hardened (required columns and distance-column handling), output contract documented, and a new per-function test file added with happy-path and invalid-input coverage.
- `fit_varhp`: input contract hardened, failure mode changed to explicit aborts with actionable messages, injectable modelling hooks added for unit tests, and a new per-function test file added with happy-path and invalid-input coverage.
- `compute_hvarpart_permutation_significance`: input contract hardened, progress output correctly gated by `verbose`, injectable backend hook added for unit tests, and a new per-function test file added with happy-path and invalid-input coverage.
- `fit_rdacca_hierarchical_partitioning`: upfront argument validation hardened and per-function tests added for invalid-input contracts.
- Enforcement gate: added lightweight contract-coverage checker and wired it into `R/testing/run_all_tests.R` to flag missing roxygen, validation, or matching test files.

### Deferred By Design

- none currently deferred

Historical note: some functions above still depend on archived packages,
downloads, or integration-heavy geospatial workflows, so tests currently focus
on contract validation and invalid-input coverage rather than full integration
execution.

## Notes

- Keep one test file per function: `test-<function_name>.R`.
- Prefer small base-R fixtures when possible so tests stay easy to run in a minimal session.
- Update this file whenever a new test file lands or a function is intentionally deferred.
