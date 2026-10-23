# Joint human-proxy HVarPart sensitivity pipeline

## Objective

Create an opt-in H1 sensitivity analysis that repeats the controlled spatial
and temporal analyses while treating square-root SPD, KK10, and square-root
HYDE as one multivariate human-predictor group. The canonical H1 analyses,
stores, figures, and manuscript remain unchanged.

## Analysis contract

- Import the fully matched Issue 342 proxy product rather than extracting the
  rasters again.
- Analyse the shared 8.0--2.0 ka BP interval without extrapolation.
- Use `spd_transformed`, `kk10_transformed`, and `hyde_transformed` as the human
  group and retain the four canonical climate predictors.
- Retain canonical fitting settings except for a sensitivity-specific minimum
  of four residual degrees of freedom in the within-dataset time-control
  models. This is required because 13 ages and eight predictors plus an
  intercept leave four residual degrees of freedom in a full-rank design.
- Reproduce the Figure 3 analytical sequence: time control within records,
  human-minus-climate balance, then spatial dbMEM aggregation.
- Reproduce the Figure 4 analytical sequence: region-by-age HVarPart with
  spatial dbMEM control.
- Display only the joint human-proxy model in the temporal figure.

## Implementation

- Use `sensitivity_analyses/joint_human_proxy_hvarpart` as a dedicated external
  target store.
- Fingerprint the canonical H1 inputs and the public
  `data_human_proxy_matches` target.
- Fail with the exact acquisition and convergence runner commands when the
  matched proxy target is unavailable.
- Keep preparation, plotting, and export logic in documented functions under
  `R/functions/`; the pipeline only orchestrates targets.
- Register two sensitivity profiles and the new pipeline contract.
- Export exactly two primary figures in PNG and PDF, plus complete numerical
  source, status, diagnostic, coverage, and provenance tables.

## Validation

- Test unique joins, transformations, age limits, region agreement, duplicate
  failures, multi-column human predictor groups, plotting, and exporters.
- Validate the target manifest before fitting and run the pipeline restartably.
- Reconcile current-input expectations: 14,802 joined rows, 1,175 datasets,
  13 ages, 65 region-age groups, 841 full-rank within-record designs, and 65
  full-rank spatial designs.
- Require explicit statuses with no silent model errors, reconstruct plotted
  values from exported tables, inspect both figures, and run repository checks.

## Reproducibility boundaries

- Do not overwrite canonical H1 outputs or target stores.
- Do not commit external rasters or target state.
- Add no package dependency.
- Leave changes uncommitted and unpushed for human review.
