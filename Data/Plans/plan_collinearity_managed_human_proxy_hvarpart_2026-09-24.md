# Collinearity-managed joint human-proxy HVarPart sensitivity

## Purpose

This opt-in sensitivity analysis produces one HVarPart result in which the
human block begins with square-root SPD, KK10 land-use fraction, and
square-root HYDE population. Redundant predictors are removed locally before
model fitting. The analysis does not compare proxies as competing models and
does not modify the canonical H1 analyses.

## Locked design

- Use matched observations from 2,000--8,000 cal yr BP without extrapolation.
- Human candidates, in preference order: `spd_sqrt`, `kk10_fraction`, and
  `hyde_sqrt`.
- Climate candidates, in preference order: `temp_annual`, `temp_cold`,
  `prec_summer`, and `prec_win`.
- Select locally and independently within the human and climate blocks using
  `collinear` 3.0.2, `max_cor = 0.8`, and `max_vif = 5`, without a response.
- Retain at least one human and one climate predictor per analytical unit.
- Add time or selected dbMEM controls only after focal predictor selection.
  Diagnose, but do not automatically remove, cross-block or control
  collinearity.
- Retain the canonical minimum of 10 unique ages, four temporal residual
  degrees of freedom, and canonical spatial requirements.

## Outputs

The isolated pipeline and store are
`R/analyses/91_sensitivity_analyses/human_proxy_hvarpart_collinearity_managed/`
and `sensitivity_analyses/human_proxy_hvarpart_collinearity_managed`.
It produces exactly two primary figures: the Figure-3-style spatial balance
and Figure-4-style temporal composition for the single filtered joint model.
Audit tables expose the retained and excluded variables, correlations, VIFs,
condition indices, eligibility, fitted components, residual dependence, and
filtered-versus-unfiltered joint-model differences. Existing unfiltered joint
evidence remains explicitly labelled as reference material.
