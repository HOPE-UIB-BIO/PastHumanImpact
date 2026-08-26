# Insertion-ready manuscript patch: explained variation and human importance

Extended Data figure numbers are placeholders until manuscript integration.

## Methods insertion

### Explained variation and human importance

For each H1 model, we extracted direct commonality fractions from the
`Var.part` component of `rdacca.hp::rdacca.hp()`.
We report total adjusted R², variation unique to human impact, variation
unique to climate, variation shared by both predictor groups, and unexplained
variation.
Signed adjusted fractions are primary because negative commonality fractions
are valid adjusted partitioning results rather than negative ecological
effects.
All available models reconstructed total adjusted R² and one within an
absolute tolerance of 0.001.
A complete bounded decomposition sensitivity is supplied in the source
tables.

Hierarchical individual importance was extracted separately from `Hier.part`.
For the fit-importance relationship, we retained signed human importance and
divided it by total adjusted R².
We did not zero-truncate individual importance.
We tested association between adjusted R² and signed human relative importance
using Spearman rank correlation, with Pearson correlation as a linear
sensitivity.
Panel correlations required at least three models and non-zero variation in
both variables.

The H1 response is a joint multivariate set of ten pollen-derived assemblage
properties, not separate fits for each property.

## Results insertion

Direct decompositions were available for 1,565 of 2,679 H1 model records, and
none exceeded the 0.001 accounting tolerance.
For 1,156 available spatial SPD decompositions, median adjusted R² was 0.355
(interquartile range 0.231 to 0.482; mean 0.352).
We compare record-level distributions rather than sums because a sum of
adjusted R² scales directly with the number of models in a group and therefore
does not provide a sampling-independent comparison of model fit.

Across 1,118 importance-eligible spatial SPD models, adjusted R² showed little
association with signed human relative importance (Spearman ρ = 0.022,
Pearson r = -0.021; Extended Data Fig. Y).
Continent and climate-zone panels showed unequal sample sizes; correlations
were unavailable with fewer than three models or no variation
(Extended Data Fig. Z).

## Proposed figure captions

**Extended Data Fig. X | Distribution of adjusted R² across continents and
climate zones.**
Points show individual signed adjusted R² values for spatial SPD models.
Boxes show medians and interquartile ranges, and violins show distribution
shape where at least three varying values are available.
Annotations report model counts.
Finite negative adjusted R² values are retained.
Maps show contributing records coloured by climate zone.

**Extended Data Fig. Y | Adjusted R² and signed human relative importance
across spatial SPD models.**
Each point is an importance-eligible joint ten-response model, coloured by
climate zone.
The line and band show a linear fit and uncertainty.
Annotations report n, Spearman ρ, and Pearson r.
No zero-truncated importance values are shown.

**Extended Data Fig. Z | Adjusted R² and signed human relative importance by
continent and climate zone.**
Five continent rows and eleven climate-zone columns use the same values and
axis limits as Extended Data Fig. Y.
Populated panels report n and Spearman ρ.
Statistics and trend lines are omitted with fewer than three models or no
variation; absent combinations remain explicit empty cells.

## Source files

- `Outputs/Tables/Diagnostics/HVarPart/hvarpart__variance_decomposition__models.csv`
- `Outputs/Tables/Diagnostics/HVarPart/hvarpart__variance_summary__overall.csv`
- `Outputs/Tables/Diagnostics/HVarPart/hvarpart__variance_summary__spatial.csv`
- `Outputs/Tables/Diagnostics/HVarPart/hvarpart__variance_summary__temporal.csv`
- `Outputs/Tables/Diagnostics/HVarPart/hvarpart__variance_decomposition__missing_negative_audit.csv`
- `Outputs/Tables/Diagnostics/HVarPart/hvarpart__spatial__adjusted_r2_distribution__records.csv`
- `Outputs/Tables/Diagnostics/HVarPart/hvarpart__spatial__adjusted_r2_distribution__climate_zones.csv`
- `Outputs/Tables/Diagnostics/HVarPart/hvarpart__spatial__adjusted_r2_distribution__continents.csv`
- `Outputs/Tables/Diagnostics/HVarPart/hvarpart__human_importance__adjusted_r2__model_values.csv`
- `Outputs/Tables/Diagnostics/HVarPart/hvarpart__human_importance__adjusted_r2__overall_statistics.csv`
- `Outputs/Tables/Diagnostics/HVarPart/hvarpart__human_importance__adjusted_r2__grid_statistics.csv`
