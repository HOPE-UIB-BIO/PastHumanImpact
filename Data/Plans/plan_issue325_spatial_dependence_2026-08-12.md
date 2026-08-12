# Issue #325: spatial-dependence robustness and explicit spatial control

## Objective

Test whether the H1 human-versus-climate conclusion, especially Figure 2,
persists after reducing or modelling spatial dependence among pollen cores.
This is a reviewer sensitivity analysis and does not replace the corrected
primary H1 models.

## Analysis design

1. Diagnose and filter spatial autocorrelation in the core-level Figure 2
   importance balance with continent-specific distance-based Moran eigenvector
   maps (dbMEMs), conditional on region-by-climate-zone strata.
2. Recalculate the Figure 2 summaries after random within-stratum thinning at
   250 km and 500 km, plus leave-one-region-out and
   leave-one-climate-zone-out checks.
3. For the cross-site region-by-age H1 models, add selected dbMEMs as a third
   predictor group and report both hierarchical contributions and pure partial
   adjusted R-squared fractions.
4. Diagnose residual spatial autocorrelation rather than assuming that the
   correction succeeded. Retain explicit non-estimable and unresolved statuses.

## Reproducibility and reporting

- Use a dedicated targets pipeline and external target store.
- Reuse prepared PAP and predictor inputs; do not overwrite primary H1 targets.
- Use 100 thinning repetitions, 999 permutations, alpha 0.05, and the project
  seed.
- Export analysis-ready tables, supplementary figures, provenance, and a
  reviewer-response report.
- Preserve signed adjusted R-squared fractions; never replace negative values
  with arbitrary positive constants.

## Validation

Add contract-first unit tests for coordinate validation, great-circle
distances, dbMEM construction and selection, Moran diagnostics, thinning,
importance aggregation, robustness classification, and spatial HVarPart.
Parse the new and existing H1 manifests, run focused tests, then the complete
test suite. Full data-dependent model execution remains an explicit reviewer
pipeline run because its prepared data and target store live externally.
