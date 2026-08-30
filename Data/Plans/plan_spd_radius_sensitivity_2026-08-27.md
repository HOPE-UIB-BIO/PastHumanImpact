# SPD radius sensitivity

## Objective

Implement Issue 327 as a supplementary H1 sensitivity analysis comparing
strict 250 km and 500 km archaeological SPD search radii while preserving the
canonical 250 km-with-500 km-fallback analysis and its main figures.

## Analysis contract

- Calculate matched 250 km and 500 km SPD series for every dataset from the
  same radiocarbon snapshot, calibration curves, age grid, smoothing setting,
  minimum-date rule, and non-normalised SPD method.
- Keep raw calculations restartable in a dedicated external targets store.
- Hold PAPs, climate predictors, HVarPart methods, temporal control, spatial
  control, aggregation rules, seeds, and reporting profiles constant.
- Treat matched analytical units as the primary comparison and retain
  all-available results as coverage diagnostics.
- Define material changes as human-versus-climate ranking reversals, changes in
  estimability, or changes in the qualitative robustness classification.
  Report continuous balance differences without an arbitrary threshold.
- Do not implement site-specific dynamic radii or rerun H2.

## Implementation phases

### Phase 1 - Matched SPD products

Add a profile-aware SPD targets pipeline under data preparation. Calculate both
radii in the same per-dataset dynamic branch and publish radius-specific nested
series, coverage, and provenance. Keep the historical SPD artifacts unchanged
and use them only for regression checks.

Validation: parse the pipeline, generate its manifest, run focused function
tests, confirm unique dataset-radius keys and identical age grids, reproduce
the historical 250 km series within tolerance, and verify that valid 250 km
coverage is a subset of valid 500 km coverage.

### Phase 2 - Predictor and H1 profiles

Add strict-radius sensitivity profiles for the within-dataset time control,
time-slice spatial control, and spatial aggregation operations. Extend predictor
preparation with radius-keyed public data while leaving canonical predictor
targets unchanged. Add one profile-driven sensitivity graph in its own external
store and reuse the canonical H1 fitting and summarising functions.

Validation: generate predictor and sensitivity manifests, confirm non-SPD
predictors are identical across radius profiles, run focused targets for both
radii, verify deterministic profile-separated provenance, and confirm that no
Bayesian temporal fitting is invoked.

### Phase 3 - Paired comparison

Pair spatial results by dataset and temporal results by region and age. Export
all-available and matched-cohort values, 500-minus-250 differences, rankings,
ranking reversals, estimability changes, geographic exceptions, signed
hierarchical contributions, zero-truncated compositions, and unique adjusted
R-squared fractions.

Validation: test key uniqueness, pairing, delta direction, missing-profile
handling, and ranking classification; independently reconstruct all summary
counts and statistics from the source targets.

### Phase 4 - Reporting

Export semantic source tables and spatial and temporal comparison figures. Add
a traceable reviewer-response Quarto document and manuscript-ready Methods,
Results, Discussion, and caption text. Register the final evidence in the
reporting manifest.

Validation: reconcile artifacts with public targets, render and visually inspect
the reviewer response and figures, run focused and full tests, generate all
affected manifests, parse changed R files, and confirm the locked R environment
remains synchronized.

## Reproducibility boundaries

- Use `Targets_data/data_preparation/spd` and
  `Targets_data/sensitivity_analyses/spd_radius` for new external state.
- Do not overwrite canonical H1 stores, generated main figures, historical SPD
  artifacts, or temporal-model fits.
- Add no new package dependency.
- Leave all repository changes uncommitted and unpushed for human review.
