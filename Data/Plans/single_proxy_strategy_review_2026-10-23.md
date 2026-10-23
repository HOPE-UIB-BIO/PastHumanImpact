# Stress test of the one-model-per-proxy strategy

Companion to `Data/Plans/human_proxy_conceptual_assessment_2026-10-23.md` (section 8, working direction) and `Data/Plans/plan_human_impact_reframing_2026-10-23.md`.
Status: internal pre-review, written as a critical reviewer would, before any of the new runs.

The strategy under review: every model contains exactly one human proxy (SPD, KK10, or HYDE 3.5 land use in the main comparison), climate is reduced by a response-free collinearity filter shared by all proxies in a unit, and the spatial and temporal H1 (and H2) analyses are repeated for each proxy.
The issues are ranked by how damaging each could be. Each gives the reviewer's objection, our best answer, and the action taken in the plan.

## Summary

| # | Issue | Severity | Plan action |
| --- | --- | --- | --- |
| 1 | The hypothesis is no longer falsifiable | High | Per-construct hypotheses, decision metric, and headline rule written down before Phase 3 |
| 2 | Single-proxy models suffer from omitted-variable bias | High | Association language; S3 overlap reported next to the single-proxy results |
| 3 | The result depends on how climate was reduced | High | Climate-representation sensitivity (all four, filtered, principal components) |
| 4 | The "independent" proxies are not independent | Medium | KK10 and HYDE counted as one evidence family and used as a scenario bracket |
| 5 | The gridded proxies contain climate | Medium | Human ∩ space and human ∩ time diagnostics required; proxy–climate correlations reported |
| 6 | Proxy comparison is confounded by spatial scale | Medium | Scale-matched sensitivity at the SPD buffer |
| 7 | Interpolated, smooth predictors inflate within-record fits | Medium | Native-step sensitivity; human ∩ time diagnostic required |
| 8 | Linear partitioning favours the most linear proxy | Low–medium | Transformation sensitivity; linearity stated as a limitation |
| 9 | Proxy uncertainty is ignored | Low–medium | SPD resampling if feasible; otherwise a stated limitation |
| 10 | Three answers instead of one | Medium (editorial) | Main figures and abstract built around agreement across proxies |
| 11 | Proxy comparisons mix construct with sample composition | Medium | Common-cohort, common-window version of every main comparison as a supplement |

## 1. "The hypothesis is no longer falsifiable"

> With three proxies and two scales, any pattern can be described as a result. If all proxies favour climate, climate wins; if they disagree, attribution is 'construct-dependent'. What outcome would have rejected H1?

- **Answer:** the hypotheses must be restated per construct, with predictions fixed before the runs. For example, H1a: demography explains more than climate; H1b: land use explains more than climate. Each is tested at both scales, and the headline rule decides how the results combine.
- **Action:** write the construct-specific hypotheses, the decision metric (for example, the sign and interval of the dbMEM-adjusted balance), and the headline rule into the plan before Phase 3. Report all runs.

## 2. "Each single-proxy model suffers from omitted-variable bias"

> A model with only SPD attributes to 'demography' everything correlated with it, including land use and climate-driven settlement. The KK10 model does the same in reverse. Neither share is the effect of its construct.

- **Answer:** correct, and the same is true of every observational partitioning. Shares are variation *associated with* a representation, not causal effects. The S3 overlap analysis quantifies how much the demography and land-use shares are the same variation.
- **Action:** use association language throughout, and report S3 next to the single-proxy results rather than deep in the supplement.

## 3. "The result now depends on how you reduced climate"

> R1 used four climate variables; you now drop collinear ones to save degrees of freedom. Dropping a climate variable can only reduce the climate share, which biases the comparison towards humans. And why should climate be represented by one or two variables while 'human' is one? The balance is a function of how many variables each block gets.

This is the most technical and probably the most dangerous point, because the KK10 and HYDE runs that moved towards humans would also gain from any loss of climate capacity.

- **Answer:** the climate selection is made without the response and is identical across proxies, so it cannot favour one proxy over another. It can still shift every human–climate balance.
- **Action:** add a climate-representation sensitivity:
  - all four climate variables, where the residual df allow;
  - the filtered selection (main);
  - climate principal components (keeping the information of all four variables in fewer dimensions instead of dropping variables).

  Report how much the balance moves between them. If the conclusions hold across all three, the critique fails.

## 4. "Your 'independent' proxies are not independent"

> KK10 and HYDE both derive from the McEvedy and Jones population estimates. Their agreement is not replication.

- **Answer:** agreed. There are two families of evidence, not three: archaeological (SPD) and historical-demographic modelling (KK10, HYDE). KK10 and HYDE differ mainly in their land-use assumptions, which is why they serve as a scenario bracket rather than as replicates.
- **Action:** say this explicitly in Methods. Do not count KK10–HYDE agreement as triangulation.

## 5. "The gridded proxies contain climate"

> KK10 allocates land use with present-day suitability maps (climate and soil) and, before 3 ka, uses a climate-driven population model. HYDE weights grazing with BIOME and NPP. Your 'human' share may partly be climate counted twice, or smooth spatial structure.

- **Answer:** this is a property of the proxies, not of the single-proxy design, and it affects any design that uses them.
- **Action:** keep the human ∩ space and human ∩ time diagnostics with detrended variants as required. Also report each proxy's correlation with the selected climate variables per unit. Settle the GLUES question in Phase 1.

## 6. "The proxy comparison is confounded by spatial scale"

> SPD is summed over 250–500 km, while KK10 and HYDE are extracted at the pollen-source scale. Differences between proxies mix construct and scale.

- **Answer:** the scale difference is deliberate, since each proxy is extracted at the best scale its data allow. The confound can be measured.
- **Action:** add a scale-matched sensitivity with KK10 and HYDE averaged over the SPD buffer. The PR #343 extraction already provides this. If the KK10–SPD contrast persists at matched scale, it reflects the construct.

## 7. "Interpolated and smooth predictors inflate within-record fits"

> HYDE has millennial steps before 1 CE; interpolating to 500-year steps creates artificial smoothness. With about 13 ages per record and a smooth proxy, spurious trend fitting is likely, even with time controlled.

- **Answer:** partly valid, especially for spatial H1.
- **Action:** add a native-step sensitivity using only ages with exact proxy layers. Treat the human ∩ time diagnostic as a requirement for publication, not an option.

## 8. "Linear partitioning favours whichever proxy is most linearly related"

> HVarPart uses linear (db)RDA. Land-use fractions likely act through thresholds. Your comparison may rank proxies by linearity, not by importance.

- **Answer:** a fair limitation shared with R1.
- **Action:** add a transformation sensitivity (rank or log-type transforms for every proxy), and state the linearity assumption as a limitation.

## 9. "Proxy uncertainty is ignored"

> SPDs have calibration and sampling uncertainty; KK10 and HYDE are single realisations of uncertain models. The attributions are presented as if the predictors were known exactly.

- **Answer:** the KK10–HYDE bracket covers scenario uncertainty in land use. Uncertainty within each product is not propagated.
- **Action:** if feasible, add an SPD resampling sensitivity (bootstrapped date sets) for one scale. Otherwise state it as a limitation.

## 10. "Three answers instead of one"

> The editor asked for a compelling case. The paper now reports several partly conflicting results. What is the take-home message?

- **Answer:** the take-home is the pattern of agreement itself: what is robust across representations of people and what depends on them.
- **Action:** build the main figures around agreement (the agreement map and the multi-column temporal figure), and write the abstract from the headline rule.

## 11. "Your proxies are compared on different data"

> The temporal figure shows each proxy on its own records and time span. SPD covers fewer records and stops at 2 ka; HYDE extends to 0.5 ka and to the early Holocene. Differences between columns may reflect which records and periods enter, not how people are represented.

- **Answer:** showing each proxy with its full data is deliberate: it uses all the evidence each proxy offers and keeps the whole-Holocene extension, as the current SPD-versus-events figure already does. The confound can be measured.
- **Action:** add a common-cohort, common-window (8.0–2.0 ka) version of every main comparison as a supplement, and require the headline rule to hold there as well as in the main results.
