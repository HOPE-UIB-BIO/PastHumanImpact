# Insertion-ready manuscript patch: reciprocal H1 control and relative importance

Analysis-derived statements in this patch are traceable to:

- `Outputs/Tables/Diagnostics/Spatiotemporal_dependence/`
- `Outputs/Tables/H1/Spatial/SPD/`
- `Outputs/Tables/H1/Spatial/Events/`
- `Outputs/Tables/H1/Temporal/HVarPart/`
- `Outputs/Tables/Diagnostics/HVarPart/`

## Methods

### Hierarchical variation partitioning

We quantified the relative importance of human and climate predictor groups
using hierarchical variation partitioning. We retained the unmodified signed
hierarchical contributions and the pure adjusted R-squared fractions from
partial variation partitioning. Negative adjusted fractions were not replaced
with arbitrary positive constants. For bounded main-figure presentation, we
set negative hierarchical contributions to exactly zero and rescaled the
remaining contributions to sum to one. Signed hierarchical contributions and
pure conditional fractions are reported separately because they answer
related but different questions.

### Temporal control within pollen cores

Before fitting the within-core H1 models, we enforced one analytical row per
pollen core and 500-year age. Repeated response estimates at the same age were
averaged only after verifying that coordinates and predictors were invariant.
The reference model contained human and climate predictor groups. The
controlled model added centred and scaled linear age as a third structural
group. We used linear age rather than temporal MEMs because individual cores
contained at most 14 unique ages and temporal MEMs would consume excessive
residual degrees of freedom.

We diagnosed temporal dependence on the first three residual RDA axes at 500
years, 1,000 years, and the temporal connectivity threshold using ordered
cyclic-shift and mirror permutations. Models with insufficient unique ages,
incomplete predictors, rank deficiency, or insufficient residual degrees of
freedom remained explicitly non-estimable.

### Spatial aggregation of the within-core balance

For each eligible core, we calculated the bounded human-climate balance as

$$
\frac{\max(I_H,0)-\max(I_C,0)}
     {\max(I_H,0)+\max(I_C,0)},
$$

where $I_H$ and $I_C$ are the time-controlled hierarchical contributions.
Cores for which both contributions were non-positive were excluded from this
bounded presentation profile but retained in the signed diagnostic outputs.

Because coordinates are constant within a core, spatial structure was
controlled when aggregating the core-level balances. We constructed positive
distance-based Moran eigenvector maps independently within continents and
selected terms from the signed time-controlled human-minus-climate difference,
conditional on continent-by-climate-zone strata. Pooled, continental, and
climate-zone summaries were evaluated at dbMEM equal to zero. Moran's I was
diagnosed before and after filtering at 250 km, 500 km, and the dbMEM
connectivity threshold.

For the primary SPD analysis, we additionally repeated the aggregation after
100 within-stratum spatial thinnings at minimum distances of 250 km and 500 km
and after omitting each continent and climate zone in turn. We classified the
SPD result prospectively as robust, mixed, or spatiotemporally sensitive. The
event analysis used the same temporal control and spatial aggregation as an
alternative supplementary comparison but was not assigned the SPD thinning-based
robustness classification.

### Spatial control within time slices

For every region-by-age model, we retained one row per pollen core. We selected
positive dbMEMs against the multivariate PAP response conditional on the
complete human and climate predictor groups, using a global test and Blanchet
double stopping. When no conditional spatial signal was detected, the original
human-climate contributions were retained and the displayed spatial
contribution was set to exactly zero. Non-estimable models remained missing.

We fitted parallel two-group human-climate and three-group
human-climate-space models. The main temporal figure reports exact
zero-truncated hierarchical compositions. Supplementary outputs report signed
hierarchical contributions, pure human, climate, and spatial adjusted
R-squared fractions, selected spatial degrees of freedom, ranking changes, and
residual spatial autocorrelation.

## Results

### Spatial H1 result

The primary SPD conclusion remained climate-dominated after temporal control
within cores and spatial filtering during aggregation. Both signed and
zero-truncated profiles retained the original direction, all continent and
climate-zone leave-out analyses agreed, and 100% of the 250-km and 500-km
thinning replicates retained the climate-dominated ranking. Both profiles
therefore met the predeclared robust criterion. Residual short-range spatial
dependence remained detectable and is reported as a limitation rather than
treated as eliminated.

[Source: `spd__human_climate_balance__robustness__time_and_space_control.csv`,
`spd__human_climate_balance__sensitivity__time_and_space_control.csv`, and
`spd__human_climate_balance__moran_diagnostics__time_and_space_control.csv` in
`Outputs/Tables/Diagnostics/Spatiotemporal_dependence/`.]

The alternative supplementary event analysis supported the same direction. The overall
time-controlled balance was -0.059 for the signed profile and -0.174 for the
zero-truncated profile. No conditional spatial signal was selected for the
aggregated event balance, so no event dbMEM terms were added. These event
results provide an alternative supplementary comparison and are not described using the SPD
thinning-based robustness classification.

[Source:
`Outputs/Tables/H1/Spatial/Events/events__human_climate_balance__climate_zone_values__time_and_space_control.csv`.]

### Temporal H1 result

Adding selected spatial terms within region-by-age models did not reverse any
estimable human-climate ranking: 0 of 70 eligible SPD rankings and 0 of 76
eligible event rankings changed. Spatial terms were selected in 46 SPD and 56
event models. Positive residual spatial autocorrelation remained in 50 SPD and
62 event region-by-age groups. The human-versus-climate ordering was therefore
stable under the implemented spatial control, but the residuals were not
assumed to be spatially independent.

[Source: `spd_events__human_climate_space__rankings__space_control.csv`,
`spd_events__human_climate_space__status__space_control.csv`, and
`spd_events__human_climate_space__residual_moran__space_control.csv` in
`Outputs/Tables/Diagnostics/Spatiotemporal_dependence/`.]

Linear age reduced but did not eliminate within-core temporal dependence.
Residual positive temporal autocorrelation remained in six estimable SPD core
models and one estimable event core model. These models remain explicitly
flagged in the supplementary diagnostics.

[Source:
`Outputs/Tables/Diagnostics/Spatiotemporal_dependence/spd_events__human_climate_time__residual_moran__time_control.csv`.]

## Figure 2 caption

**Relative human-versus-climate importance across pollen cores after temporal
and spatial control.** Values are the bounded difference between
zero-truncated and renormalised human and climate shares from within-core models
that include linear age as a structural control. Negative values
indicate greater climate importance, zero indicates equal importance, and
positive values indicate greater human importance. Points and distributions
show observed time-controlled core values. Continental and climate-zone
markers are spatially filtered estimates evaluated at dbMEM equal to zero.
Maps show the locations and balances of contributing cores. The formal
thinning and leave-out robustness classification applies to the primary SPD
analysis.

## Figure 3 caption

**Composition of positive human, climate, and spatial hierarchical
contributions through time.** Paired
columns within each 500-year time slice show the primary SPD analysis and the
supplementary event-proxy comparison. Bars contain zero-truncated and
renormalised shares for human impact, climate, and selected spatial dbMEMs and
sum to one for every estimable model. Space is shown as exactly zero
only when no conditional spatial signal was selected; non-estimable models are
missing. SPD is shown from 2 to 8.5 ka BP and events from 0.5 to 8.5 ka BP.
Human impact and climate are the substantive predictors, whereas space is a
structural control.

## Supplementary figure captions

### Supplementary event spatial comparison

Time-controlled human-versus-climate balance using archaeological event data
as the independent human-impact proxy. The layout and bounded balance follow
Figure 2. No conditional spatial signal was selected for the aggregated event
balance; this figure is an alternative directional comparison rather than a
second thinning-based robustness analysis.

### Signed spatial component profiles

Unmodified signed hierarchical contributions of human impact, climate, and
linear age in the within-core models. Each panel retains the Figure 2 spatial
layout. Negative values are valid adjusted partitioning results and do not
represent negative ecological effects.

### Pure spatial component profiles

Pure human, climate, and linear-age adjusted R-squared fractions from partial
variation partitioning of the within-core models. These conditional fractions
are shown separately from hierarchical contributions and may be negative.

### Signed temporal profiles

Unmodified signed hierarchical contributions of human impact, climate, and
selected spatial dbMEMs through time for SPD and event models. Values are not
truncated or rescaled to a bounded composition.

### Pure temporal profiles

Pure human, climate, and selected spatial adjusted R-squared fractions through
time. These conditional fractions are not interchangeable with the
hierarchical contributions in Figure 3.

## Limitations

The reciprocal controls test whether the human-versus-climate conclusion is
stable after accounting for broad temporal and spatial structure. They do not
establish causal effects of time or space, and they do not guarantee temporal
or spatial independence. Temporal dependence within irregular original pollen
levels below the 500-year analytical resolution remains outside this analysis.
All non-estimable models and residual-dependence flags remain available in the
machine-readable supplementary tables.

## Additional analysis and correction

During quality-control checks, we identified that a previous
post-processing step did not retain negative adjusted hierarchical
contributions. This issue was identified by the authors rather than raised by
the reviewers. We corrected the extraction procedure, retained signed results
as authoritative diagnostics, and use explicitly labelled zero-truncated
compositions only for bounded main-figure presentation. The signed,
zero-truncated, and exclude-negative profiles retain the same overall
human-versus-climate ordering.
