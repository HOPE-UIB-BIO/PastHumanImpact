# Test Progress

This file tracks the project-wide test rollout for the `testthat` suite under `R/tests/testthat`.

## Current Status

- Test runner: complete
- Smoke test: complete
- Contract-first rollout: complete
- Contract coverage gate: complete (all checked functions have roxygen, argument assertions, and tests)
- Latest full-suite validation: `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 427 ]`, `EXIT_CODE=0`

### Active Batches

- none (rollout complete)

### Fresh Starting Points

- none currently listed

### Completed In This Iteration

- `get_chelsa_download`: input contract hardened for required download metadata columns and control argument types; new per-function tests added for invalid-input coverage.
- `get_climate_data`: input contract hardened for selected-variable vectors and required coordinate columns in `xy`; new per-function tests added for invalid-input coverage.
- `get_events_spd_combined`: contract expanded (including roxygen params/return) with required input-shape validation; new per-function tests added for invalid-input coverage.
- `fit_brms_hgam`: roxygen contract added to document modelling intent, arguments, and failure return behavior.
- `get_density_pap_combined`: input contract hardened for required source/meta/dummy-time columns and logical controls; tests expanded with explicit invalid-input coverage.
- `get_data_m2`: roxygen contract added, input contract hardened for source/meta shape and nested payloads, and a new per-function test file added with happy-path and invalid-input coverage.
- `run_pca`: input contract hardened for source table shape and `scale` flag, roxygen contract expanded, tests expanded with explicit invalid-input coverage.
- `run_pcoa`: input contract hardened for matrix shape and NA handling, roxygen contract expanded, tests expanded with explicit invalid-input coverage.
- `run_dbrda`: roxygen contract added and input contract hardened for matrix/predictor shape on non-NULL execution path while preserving NULL short-circuit behavior; tests expanded with invalid-input coverage.
- `extract_m2_time`: input contract hardened for matrix and square-shape requirements; tests expanded with explicit invalid-input coverage.
- `get_m2_time_df`: input contract hardened for atomic-vector requirements; tests expanded with explicit invalid-input coverage.
- `get_procrustes_m2`: input contract hardened for non-empty named list model inputs; tests expanded with explicit invalid-input coverage.
- `get_diversity`: input contract hardened for required source columns, nested count tables, and control arguments; tests expanded with invalid-input coverage.
- `get_dcca`: input contract hardened for required source columns and nested data payloads; tests expanded with invalid-input coverage.
- `prepare_data_cp`: input contract hardened for required source columns and nested payload shapes across all input tables; tests expanded with invalid-input coverage.
- `get_diversity_and_dcca_for_modelling`: roxygen contract added, explicit input validation added for source tables and nested payload columns, tests expanded with invalid-input coverage.
- `get_interpolated_data`: roxygen contract added, explicit input validation added for source shape, nested payloads, and selector arguments, tests expanded with invalid-input coverage.
- `predict_brms_model`: roxygen contract added, explicit model-object validation added, tests expanded with invalid-input coverage.
- `get_events_for_modelling`: roxygen contract added, explicit input validation added for source table and nested `events_updated` payloads, tests expanded with invalid-input coverage.
- `get_spd_for_modelling`: roxygen contract added, explicit input validation added for source table and nested `spd` payloads, tests expanded with invalid-input coverage.
- `get_roc_for_modelling`: roxygen contract added, explicit input validation added for source table and nested `PAP_roc` payloads, tests expanded with invalid-input coverage.
- `flag_model_to_rerun`: roxygen contract added, explicit input validation added for required config columns and selector args, tests expanded with invalid-input coverage.
- `get_all_predicted_general_trends`: roxygen contract added, explicit input validation added for required columns and storage-path availability, tests expanded with invalid-input coverage.
- `get_hgam_formula`: roxygen contract added and tests expanded with invalid-input coverage for grouping-count validation.
- `get_chelsa_trace21k_urls`: input contract hardened with explicit argument validation and explicit namespace usage for string and case helpers; tests expanded with invalid-input coverage.
- `get_climate_data_for_interpolation`: roxygen contract added, explicit input validation added for source shape and nested climate tables, deprecated selection warning removed, tests expanded with invalid-input coverage.
- `get_summary_tables`: roxygen contract added, explicit input validation added for required summary columns and grouping args, tests expanded with invalid-input coverage.
- `merge_all_events`: roxygen contract expanded, explicit input validation added for variadic data-frame inputs, tests expanded with invalid-input coverage.
- `get_pollen_data`: input contract hardened for source table and requested variable selection, tests expanded with invalid-input coverage.
- `make_dir`: roxygen contract added, explicit path-type validation added, tests aligned to explicit invalid-input failure behavior.
- `get_events_from_indicators`: roxygen contract expanded, explicit input validation added for required indicator/pollen/meta columns and selector arguments, tests expanded with invalid-input coverage.
- `get_events_from_indices`: roxygen contract expanded, explicit input validation added for required index/pollen/meta columns and selector arguments, message output gated behind `verbose`, tests expanded with invalid-input coverage.
- `get_events_as_binary`: tests expanded with additional invalid-input coverage for missing pollen columns and malformed nested `events_age` tables.
- `get_data_for_h2_hvar`: roxygen contract added, explicit input validation for required predictor and m2 columns added, tests expanded with invalid-input coverage.
- `get_data_filtered`: roxygen contract added, explicit input validation for inputs and age bounds added, `remove_private = FALSE` branch fixed to use `data_meta`, tests updated and expanded with invalid-input coverage.
- `get_data_timebin`: roxygen contract added, explicit input validation for required source/meta columns added, tests expanded with invalid-input coverage.
- `get_scores_dbrda`: roxygen contract added, explicit validation added for model-like non-atomic input (or `NULL`), tests expanded with invalid-input coverage.
- `transform_ages`: contract hardened with roxygen and explicit input validation for required source and translation columns; tests expanded with invalid-input coverage.
- `add_age_as_factor`: roxygen contract added, explicit input validation for required numeric `age` added, tests expanded with invalid-input coverage.
- `add_climatezone_as_factor`: roxygen contract added, explicit input validation for required `climatezone` and config table columns added, tests expanded with invalid-input coverage.
- `add_predictor_as_factor`: roxygen contract added, explicit input validation for required `predictor` column added, tests expanded with invalid-input coverage.
- `add_region_as_factor`: roxygen contract added, explicit input validation for required `region` column added, tests expanded with invalid-input coverage.
- `get_climatezone_label`: roxygen contract added, explicit character-input validation added, tests expanded with invalid-input coverage.
- `merge_indicators_and_indices`: roxygen contract tightened, explicit input validation added, tests expanded with invalid-input coverage.
- `add_logical_rules`: roxygen contract tightened, explicit input validation and unsupported-region failures added, message output gated behind `verbose`, tests expanded with invalid-input coverage.
- `subset_event_types`: roxygen contract tightened, explicit input validation and unsupported-region failures added, data-frame event payload handling fixed, tests expanded with invalid-input coverage.
- `get_events_as_binary`: roxygen contract tightened, explicit input validation added, message output gated behind `verbose`, tests aligned to function contract and validation behavior.
- `get_file_from_path`: roxygen contract tightened, explicit path validation added, tests expanded with invalid-input coverage.
- `get_data_properties`: roxygen contract added, explicit input validation added for required columns, data source shape, and `used_rescale`, tests expanded with invalid-input coverage.
- `get_data_combined`: roxygen contract added, explicit required-column validation added, tests expanded with invalid-input coverage.
- `get_meta_data`: explicit input validation added for assembly shape and requested variables, tests expanded with invalid-input coverage.
- `check_storage_folders`: roxygen contract added, explicit path validation added, tests expanded with invalid-input coverage.
- `get_data_predictors`: roxygen contract added, explicit input validation added, tests expanded with invalid-input coverage.
- `get_roc`: contract and validation hardened (including verbose and injectable estimation/detection hooks for unit tests), and a new per-function test file added with happy-path and invalid-input coverage.
- `run_hvarpart`: input contract hardened (required columns and distance-column handling), output contract documented, and a new per-function test file added with happy-path and invalid-input coverage.
- `get_varhp`: input contract hardened, failure mode changed to explicit aborts with actionable messages, injectable modelling hooks added for unit tests, and a new per-function test file added with happy-path and invalid-input coverage.
- `perm_hvarpart`: input contract hardened, progress output correctly gated by `verbose`, injectable backend hook added for unit tests, and a new per-function test file added with happy-path and invalid-input coverage.
- `rdacca_hp`: upfront argument validation hardened and per-function tests added for invalid-input contracts.
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
