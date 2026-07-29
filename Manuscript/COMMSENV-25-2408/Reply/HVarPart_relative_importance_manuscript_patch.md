# Insertion-ready manuscript patch: HVarPart relative importance

All reported values below are traceable to
`Outputs/Tables/HVarPart/hvarpart_profile_comparison.csv` and
`Outputs/Tables/HVarPart/hvarpart_model_audit.csv`.

## Methods

We quantified the relative importance of human and climate predictor groups
using hierarchical variation partitioning. We retained the unmodified
predictor-level Unique, Average share, Individual, and Individual percentage
components returned by HVarPart, together with each model's total adjusted
R-squared. The primary estimand was the signed allocation of adjusted explained
variation, calculated for predictor group p within reporting group g as the sum
of individual contributions divided by the sum of model total adjusted
R-squared values. We excluded models only when a required predictor group was
absent, total adjusted R-squared was non-finite or non-positive, or an
individual contribution was non-finite. We assessed sensitivity by (i) setting
negative individual contributions to exactly zero and recomputing model
denominators and (ii) excluding a complete model when any predictor contribution
was negative.

## Results

For spatial SPD models, the primary signed allocation was 0.764 for climate and
0.236 for human predictors. Climate allocation was 0.756 under exact-zero
truncation and 0.724 after excluding models with any negative individual
contribution. Climate remained larger than human predictors for spatial events,
temporal SPD, temporal events, and H2, and this ordering was unchanged under
both sensitivity profiles. [Source:
`Outputs/Tables/HVarPart/hvarpart_profile_comparison.csv`, rows with
`aggregation_level = analysis`.]

## Figure 2 caption

Signed allocation of adjusted explained variation between human and climate
predictor groups in spatial SPD models. Points show sequence-level allocations
from unmodified HVarPart individual contributions; larger outlined points show
pooled allocations calculated as the sum of individual contributions divided
by the sum of total adjusted R-squared. Shared central limits are the global
1st--99th percentiles rounded outward. Boundary markers and the subtitle report
observations outside those display limits; all observations are retained in the
source table and full-range supplementary figure. Negative allocations are
adjusted partitioning results and do not indicate negative ecological effects.

## Figure 3 caption

Signed allocations of adjusted explained variation through time for H1 models.
Human and climate allocations are shown as unstacked points for SPD and event
predictors. Horizontal lines mark zero and one; values are not truncated to a
bounded proportion scale.

## Figure 4 caption addition

Inset bars show unstacked signed allocations of adjusted explained variation
for human and climate predictors. Horizontal reference lines mark zero and one.

## Limitations

Signed hierarchical-partitioning allocations can be negative or greater than
one because adjusted shared contributions, suppression, collinearity, weak
information, and sampling variation affect the allocation. A negative
importance value is not a negative ecological effect. Although sensitivity
profiles changed the magnitude of pooled allocations, they did not change the
climate-versus-human ordering in any analysis. Model eligibility and all
exclusion reasons are reported in
`Outputs/Tables/HVarPart/hvarpart_model_audit.csv`.

## Reviewer response

We corrected the post-processing so that fitted HVarPart results are preserved
exactly. The revised primary summary uses signed individual contributions and
excludes only models that cannot support the estimand. We additionally report
exact-zero and negative-model-exclusion sensitivities. Climate remains the
larger predictor group under all three profiles in every analysis. Figure 2 has
been recreated from the corrected data with explicit signed labeling, common
central limits, tail counts, and a full-range supplement. A complete redesign
of Figure 2 is deferred until the corrected recreation is reviewed.
