# Temporal Predictor Standardisation

## Summary

Standardize every configured model predictor using statistics from that
model's fitting subset. Correct the common smooth basis to `"cr"`, introduce
auditable response checks, and invalidate all existing fits because their
formulas change.

## Implementation Changes

- Extend model specs and config with the original and standardised predictor
  names, model-specific mean and standard deviation, smooth settings,
  requested and effective profiles, eligibility, and validation statistics.
- Standardize predictors as `(x - x_mean) / x_sd` and require finite values
  with a positive standard deviation.
- Refactor fitting and prediction to apply the same stored transformation.
- Generate formula text from the resolved config so displayed and fitted
  formulas are identical.

## Response Checks

- Retain constant-response data until config creation so exclusions remain
  auditable.
- Mark globally constant responses ineligible.
- Replace dataset factor smooths with a common smooth and dataset random
  intercept when no dataset has within-dataset response variation.
- Record fallback and exclusion reasons in the config table.

## Predictions And Lifecycle

- Build prediction grids in original age units and add the standardised model
  predictor using stored scaling statistics.
- Save original and standardised predictor columns and verify the inverse
  transformation `x = x_scaled * x_sd + x_mean`.
- Add scaling, basis, and effective-profile fields to model-definition
  comparison and invalidate all eligible existing models.
- Preserve old model files; ineligible config rows are never queued.

## Validation

- Test per-model scaling, inverse transformation, formula basis, profile
  fallback, eligibility checks, and prediction output contracts.
- Run focused tests, the complete project test suite, and regenerate temporal
  data, specs, and config without fitting models.

## Assumptions

- Only configured predictors are standardised; responses remain unchanged.
- Scaling statistics are calculated after all fitting filters.
- `"cr"` is the default common smooth basis.
- Existing fitted models are not backward compatible with the new formulas.
