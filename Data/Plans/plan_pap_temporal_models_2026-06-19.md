# Plan: PAP Trajectories And General Modelling Refactor

## Goal

Create modelled PAP-through-time figures for all PAP metrics by
`region x climatezone`, while redesigning `R/temporal_models` into a general
modelling framework that can fit one temporal model per analysis unit and share
run/evaluate/rerun tracking across predictors, events, and PAPs.

## Scope

- Keep `brms` as the modelling engine.
- Preserve the current model lifecycle pattern: config table, `need_to_run`,
  `need_to_be_evaluated`, convergence diagnostics, rerun flags, saved model
  files, and saved prediction files.
- Make that lifecycle general enough for predictors, events, and PAPs.
- Fit PAP models separately for each `region x climatezone` combination, so
  each model estimates one general trajectory across cores within that stratum.
- Refactor existing functions and scripts where they already own the behavior.
  Add new functions only when there is no suitable existing owner or when
  extracting script logic is needed to keep analytical scripts clean.
- Do not define helper functions inside analytical scripts. New reusable logic
  must live under `R/functions/`, one primary function per file, with roxygen
  and tests following `.ai/r-functions.md`.

## Saved Plan

**Goal:** Make the implementation plan a durable project artifact before code
changes.

**Tasks:**

- [x] Create `Data/Plans/plan_pap_temporal_models_2026-06-19.md`.
- [x] Include formula-research questions, validation gates, and model
  references.

**Validation:**

- Confirm the Markdown file exists and contains the full plan.

## Formula Research

**Goal:** Decide the new HGAM formula from evidence and small executable tests,
with special attention to whether core-specific wiggliness through `dataset_id`
is required.

**Tasks:**

- [x] Review HGAM guidance from Pedersen et al. 2019, `mgcv` factor-smooth
  documentation, and `brms` smooth documentation.
- [x] Use `Data/Temp/research_hgam_formula_pap.R` with a small sampled PAP
  dataset.
- [x] Compare candidate profiles for separate `region x climatezone` models:
  - Shared within-stratum smooth plus dataset random intercept.
  - Shared within-stratum smooth plus dataset random intercept and slope.
  - Shared within-stratum smooth plus dataset factor smooth with `bs = "fs"`.
  - Current-style dataset smooth comparator.
- [x] Measure formula construction, prior extraction, model compilation/fit on
  a tiny subset, prediction shape, runtime, warnings, and convergence
  diagnostics.
- [x] Record the chosen default formula and rationale in this file.

**Validation:**

- Temporary research script runs from a clean R session.
- The selected formula successfully fits a tiny representative PAP subset from
  one `region x climatezone` stratum and produces predictions.
- The chosen formula keeps full data complexity at `dataset_id` level inside
  each `region x climatezone` model.

**Research artifact:**

Research notes and results are recorded separately in
`Data/Temp/research_hgam_formula_pap_summary.qmd`.

## General Model Framework

**Goal:** Make predictor-model infrastructure reusable for predictors, events,
and PAPs.

**Tasks:**

- [ ] Add a general model-spec table with fields for `analysis`, `variable`,
  `region`, `climatezone`, `family_key`, `engine`, `model_profile`, age limits,
  minimum records, run flags, evaluation flags, diagnostics, paths, and output
  IDs.
- [ ] Replace string families plus `eval(parse())` with a tested family-mapping
  function.
- [ ] Refactor `get_hgam_formula()` into the shared formula-string engine.
- [ ] Generalise model running, evaluation, rerun flagging, and prediction
  extraction so PAPs use the same lifecycle as existing predictor/event models.
- [ ] Keep existing predictor/event behaviour readable during transition; avoid
  breaking current saved outputs unnecessarily.

**Validation:**

- Focused tests pass for model specs, family mapping, formula builder,
  config-table creation, rerun flagging, and prediction output contracts.
- Existing tests for `fit_brms_model()`, `predict_brms_model()`, and
  `get_all_predicted_general_trends()` are updated or replaced intentionally.

## PAP Model Inputs And Fitting

**Goal:** Prepare all PAP variables for modelled region-climatezone
trajectories.

**Tasks:**

- [ ] Build PAP model input from `data_properties`, joined to metadata.
- [ ] Include all PAPs: `n0`, `n1`, `n2`, `n1_minus_n2`,
  `n2_divided_by_n1`, `n1_divided_by_n0`, `roc`, `dcca_axis_1`,
  `density_diversity`, `density_turnover`.
- [ ] Filter to the agreed temporal range, exclude Africa, and apply
  `min_n_records_per_climate_zone`.
- [ ] Fit one model per PAP variable per `region x climatezone` using the
  selected formula profile and existing-style model tracking.

**Validation:**

- PAP config table contains expected variables and valid `region x climatezone`
  strata.
- A single PAP model can be run, evaluated, flagged, rerun if needed, and
  predicted through the general framework.

## PAP Figures

**Goal:** Generate all-PAP through-time figures by continent/region and climate
zone.

**Tasks:**

- [ ] Extract population-level fitted trajectories with uncertainty on the
  response scale.
- [ ] Save combined prediction tables under `Outputs/Tables/`.
- [ ] Generate one figure per PAP metric, faceted `region ~ climatezone_label`,
  with observed dataset trajectories, fitted mean, and credible ribbon.
- [ ] Save manuscript-ready `.png` and `.pdf` outputs under
  `Outputs/Figures/Extended_data_figures/`.

**Validation:**

- Figure script runs end-to-end from a clean session.
- Spot checks confirm plotted predictions match saved prediction tables.
- Focused `targets::tar_manifest()` and, where feasible, focused `tar_make()`
  checks pass for PAP and H1 temporal dependencies.

## Risks

- `bs = "fs"` at dataset level may be too slow; the research task must test
  whether factor smooths should be applied to `stratum` rather than
  `dataset_id`.
- Full `brms` fitting for all PAPs will be expensive; the generalized
  config/rerun system must support partial runs.
- Formula changes affect scientific interpretation, so the formula decision
  must be documented before full fitting.

## Assumptions

- Use `brms` first; no `mgcv` backend in the first implementation.
- Preserve existing convergence/rerun workflow, but generalise it beyond
  predictor models.
- All new functions follow repo function rules: separate files, roxygen,
  explicit returns, validation, and focused tests.
