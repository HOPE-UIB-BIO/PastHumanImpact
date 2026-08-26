# Insertion-ready manuscript patch: HVarPart relative importance

All numerical statements below are traceable to
`Outputs/Tables/Diagnostics/HVarPart/hvarpart__profile_comparison.csv` and
`Outputs/Tables/Diagnostics/HVarPart/hvarpart__model_audit.csv`.

## Methods

We quantified the relative importance of human and climate predictor groups
using hierarchical variation partitioning. We retained the unmodified
predictor-level Unique, Average share, Individual, and Individual percentage
components returned by HVarPart, together with the denominator required for
their allocation. We compared three reporting profiles. The signed profile
retained negative individual contributions. The zero-truncated profile set
negative individual contributions to exactly zero and recomputed the
denominator from the truncated contributions. The exclude-negative profile
excluded a complete model when either predictor contribution was negative.
Models were otherwise excluded only when a required predictor group was
absent, the denominator was non-finite or non-positive, or an individual
contribution was non-finite.

The main figures express paired zero-truncated human and climate allocations
as a bounded difference. Figure 2 applies this balance spatially, Figure 3
through time, and Figure 4 within its importance insets. Full-range signed
results and the exclude-negative sensitivity are reported in the
Supplementary Information.

## Results

For spatial SPD models, the zero-truncated allocations underlying the main
balance figure were 0.756 for climate and 0.244 for human impact. The
corresponding signed allocations were 0.764 and 0.236. Climate allocation was
0.724 after excluding
models with any negative individual contribution. Climate remained larger
than human impact for spatial events, temporal SPD, temporal events, and H2,
and this ordering was unchanged across all three profiles. [Source:
`Outputs/Tables/Diagnostics/HVarPart/hvarpart__profile_comparison.csv`, rows with
`aggregation_level = analysis`.]

## Figure 2 caption

Relative importance balance in spatial SPD models, calculated as the
zero-truncated human allocation minus the zero-truncated climate allocation.
The bounded scale runs from climate impact (-1), through equal importance (0),
to human impact (1). Densities show record-level balances within each
continent, points and intervals show climate-zone summaries, and coloured
horizontal lines show pooled continental balances. Density colour varies from
climate through neutral to human importance. Maps show the locations and
climate-zone classification of the analysed records.
The climate-zone summary panels use a pale climate-to-neutral-to-human
background gradient, while the maps and continental density panels do not.

## Supplementary Figure 2 caption

Signed allocation of adjusted explained variation to human impact in spatial
SPD models. The original three-part spatial geometry is retained, but
unmodified contributions are shown over their complete range. Negative
allocations are adjusted partitioning results and do not indicate negative
ecological effects.

## Figure 3 caption

Relative importance balance through time for H1 models, calculated as the
zero-truncated human allocation minus the zero-truncated climate allocation.
Lollipop stems extend from equal importance to each estimate; point shape
distinguishes SPD and event human-impact proxies, and point colour varies from
climate through neutral to human importance against a pale version of the
same gradient. SPD estimates cover 2--8.5 ka BP; event-based estimates cover
0--8.5 ka BP.

## Supplementary Figure 3 caption

Signed allocations of adjusted explained variation through time for H1
models. Human and climate allocations are shown as unstacked trajectories for
the SPD and event predictors. Horizontal lines mark zero and one; values are
not truncated to a bounded scale. SPD estimates cover 2--8.5 ka BP;
event-based estimates cover 0--8.5 ka BP.

## Figure 4 caption addition

Inset lollipops show the relative importance balance, calculated as the
zero-truncated human allocation minus the zero-truncated climate allocation.
Stems extend from equal importance to each estimate, and point colour varies
from climate through neutral to human importance against a pale version of
the same gradient. The inset is enclosed by a box and has an explicit equal-
importance line.

## Supplementary Figure 4 caption addition

Inset bars show unmodified signed allocations of adjusted explained variation
to human and climate predictors. Zero and one provide reference values; the
insets are not restricted to a bounded scale.

## Limitations

Signed hierarchical-partitioning allocations can be negative or greater than
one because adjusted shared contributions, suppression, collinearity, weak
information, and sampling variation affect the allocation. A negative
importance value is not a negative ecological effect. Although the sensitivity
profiles changed the magnitude of some pooled allocations, they did not change
the climate-versus-human ordering in any analysis. Model eligibility and all
exclusion reasons are reported in
`Outputs/Tables/Diagnostics/HVarPart/hvarpart__model_audit.csv`.

## Additional analysis and correction

During additional quality-control checks, we identified an error in our
previous post-processing: negative adjusted hierarchical-partitioning
contributions were not retained. This issue was identified by the authors
rather than raised by the reviewers. We corrected the extraction procedure and
compared signed, zero-truncated, and exclude-negative reporting profiles. All
three profiles support the same overall conclusion. We therefore retain the
explicitly labelled zero-truncated profile in the main figures and provide
full-range signed versions of Figures 2--4 in the Supplementary Information.
