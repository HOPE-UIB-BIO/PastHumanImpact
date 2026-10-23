# Human-impact reframing after the human-proxy results

## Objective

Re-plan the remaining revision work now that the representation of human impact, rather than the climate–human comparison itself, has become the decisive analytical choice.

PR #343 showed that adding KK10 land use (and HYDE population) to the archaeological SPD reverses spatial H1 from climate-dominant to human-dominant, while temporal H1 stays climate-dominant.
On 2026-10-22 the co-authors provisionally chose SPD + KK10 over 8.0–2.0 ka (`Manuscript/_internal/Paper_direction_after_proxy_decision.md`, gitignored).
That choice is reopened here:

- No single primary human proxy: each model contains one proxy, and every analysis is repeated for SPD (demography), KK10 (land use), and HYDE 3.5 land use (working direction, to confirm at G2).
- The analysis window is a design factor: a common 8.0–2.0 ka window for the controlled comparison, plus each proxy's native window towards the whole Holocene, which would be a much stronger argument than 8.0–2.0 ka alone.
- H2 must be fully refitted with the same human proxies as H1.
- All reviewer requests still stand, including #341 (300-grain refit) and the R1 validation suite, which must be rerun for each human proxy.

The plan therefore makes the H1/H2 engine proxy- and window-agnostic, and repeats every analysis with one human proxy per model, so the co-authors judge the conclusions across proxies rather than from one-off sensitivity pipelines.
This plan supersedes the provisional direction note; its analyses A–I are folded into the phases below, except the joint SPD + KK10 primary (analysis A), which the single-proxy design replaces.

## Current evidence (exploratory)

These numbers come from exploratory outputs and are not manuscript-ready; the final text must take them from the evidence registry as inline R.

| Specification | Window | Spatial: human-dominant sequences | Spatial: dbMEM-adjusted balance | Temporal: units with human > climate (of 65) |
| --- | --- | --- | --- | --- |
| Canonical SPD (R1) | 8.5–2.0 ka | 18.9 % | −0.466 | 1 |
| SPD bridge (joint cohort and climate set) | 8.0–2.0 ka | 28.5 % | — | 0 |
| SPD + KK10, unfiltered | 8.0–2.0 ka | 67.0 % | +0.167 | 0 (one tie) |
| KK10 only | 8.0–2.0 ka | 55.1 % | +0.036 | 0 |
| HYDE only | 8.0–2.0 ka | 61.6 % | +0.101 | 2 |
| SPD + KK10 + HYDE, filtered | 8.0–2.0 ka | 65.6 % | — | 18 |

The joint rows (SPD + KK10, SPD + KK10 + HYDE) are kept as the evidence that prompted the reframing; joint human models are not pursued further. The HYDE row uses HYDE 3.2 population, and only the SPD bridge used a reduced climate set, so the single-proxy rows are previews of the new design, not results from it.

Sources: `Outputs/Tables/H1/Spatial/SPD/`, `Outputs/Tables/H1/Sensitivity/{SPD_KK10_hvarpart,KK10_only_hvarpart,HYDE_only_hvarpart}/` (`*spatial_balance_estimates.csv`, `*spatial_balance_records.csv`, `*spatial_control_composition.csv`), and `Outputs/Tables/H1/Sensitivity/Human_proxy_hvarpart_collinearity_managed/` (`decision_spatial_summary.csv`, `decision_temporal_summary.csv`).

Two caveats shape the plan:

- In temporal H1 the human share mostly replaces the spatial (dbMEM) share: the median space allocation falls from 0.255 to about 0.03 when KK10 enters. Smooth land-use fields may absorb broad spatial structure.
- SPD and KK10 agree in level (Kendall tau 0.87 on SPD-decile medians) but not in change (first-difference tau 0.07; `Outputs/Tables/H1/Sensitivity/Human_proxy_convergence/correlations.csv`, `trend_sensitivity.csv`).

## Conceptual frame

### Human-impact construct

Decide what "human impact" means before choosing the proxy.
The full conceptual assessment is in `Data/Plans/human_proxy_conceptual_assessment_2026-10-23.md`. It places each proxy on the causal chain (presence → land use → realised impact → pollen signal), records which H1/H2 dimension each proxy is informative for, lists structural hazards for HVarPart, and sets out seven usage strategies (S1–S7) with a proposed role for each proxy. Of these, S5 (one proxy per model) is the working direction and S3 (demography versus land-use overlap) is kept as a supplement; S1, S2, S4, S6, and S7 are dropped.
Its central point is that the closer a proxy sits to realised impact, the less independent it is of the pollen response: SPD and HYDE population measure presence, KK10 and HYDE land use measure modelled land use, and only the circular expert events measure impact.
The summary table below scores every candidate against explicit criteria; cells marked "check" are resolved in Phase 1.

| Criterion | SPD | KK10 | HYDE 3.x | Expert events | ArchaeoGLOBE |
| --- | --- | --- | --- | --- | --- |
| Independent of pollen | yes | likely; check the Bradshaw (2004) calibration series | yes for 3.2 | no (circular) | check |
| Independent of climate | yes | check; before 3 ka the GLUES population is climate-driven | partly; BIOME/NPP weighting affects land use, not `popc` | not applicable | check |
| Temporal coverage | about 12–2 ka; unreliable after 2 ka | 8 ka–AD 1850 | 12 ka–present | full Holocene | check (about 10 ka–AD 1850) |
| Temporal resolution | continuous | annual | millennial before 1 CE | event-based | coarse time slices |
| Spatial support | 250/500 km date buffer | 5' grid | 5' grid | per record | regions |
| Construct | activity and demography | land use | population (`popc`) and land use (cropland, grazing) | local disturbance | land-use stage |

SPD-based models stop at 2 ka because radiocarbon dating declines once written sources exist, so younger SPD values reflect dating practice rather than population.

### Window strategy

No single non-circular proxy covers the whole Holocene at useful resolution: SPD ends at 2 ka, KK10 starts at 8 ka, and HYDE is coarse and, in 3.2, near zero before about 3 ka (verify for 3.5).
The window is therefore treated as a design factor with nested windows:

- Proxy-native windows with each proxy's full data: SPD about 11.7–2.0 ka; KK10 8.0–0.5 ka; HYDE 3.5 about 11.7–0.5 ka. These are the main temporal H1 results and extend the inference towards the whole Holocene.
- Core window, 8.0–2.0 ka, on a common cohort: every proxy is compared on identical units, as a check that differences between proxies are not caused by sample composition or window length.
- The HYDE 3.5 native run is the whole-Holocene run; it counts as evidence only if its early-Holocene values carry information rather than near-zero constants.

The response and climate data already cover 0–12 ka (`min_age` and `max_age` in `R/00_Config_file.R`), so only the human proxy limits the window.
Longer windows also give the within-sequence models more ages; the 8.0–2.0 ka models have 13 ages and needed the residual-df minimum relaxed to four.
The 0 ka slice is excluded for KK10 because it ends at AD 1850.

### Framing scenarios

Choose after the single-proxy runs:

1. Scale divergence: change through time within regions aligns with climate whatever the human proxy, while differences among sites depend on whether people are represented by demography or by land use (the picture in the single-proxy previews).
2. Necessary but not sufficient: people mattered, but climate remained a major, often leading, driver of assemblage-level change for every proxy.
3. Representation dependence: attribution depends on how human impact is represented at both scales, and the paper reports the results per construct.

An optional H3 (single property versus whole assemblage; analysis D) can support scenario 1 or 2.

### Decision gates

- G1, after Phase 1: candidate proxies that pass the independence and coverage checks, and the per-construct hypotheses, decision metric, and headline rule recorded before any Phase 3 run.
- G2, after Phase 3: confirm the single-proxy design (section 8 of the conceptual assessment), the proxy set, the window, the headline rule, the framing, and the roles of HYDE population and the expert events.
- G3, before Phase 6: resubmission as R1 of COMMSENV-25-2408 or as a new manuscript.

## Analysis contract

- Canonical R1 stores, figures, and temporal-model fits are not overwritten; new runs get their own stores.
- Each model contains exactly one human proxy; there are no joint human blocks (degrees of freedom, block-size asymmetry, mixed constructs; see section 8 of the conceptual assessment).
- Climate predictors are selected once per analytical unit without the response (`collinear`, `max_cor = 0.8`, `max_vif = 5`) and shared by every proxy's model in that unit; time and dbMEM controls are protected and added after selection.
- Temporal H1 main results use each proxy's full data over its native window, as in the current SPD-versus-events figure; the common-cohort, common-window version is a supplementary check of sample composition. Spatial H1 comparisons that pair proxies record by record (the agreement map) use the common cohort by construction.
- The headline rule is fixed before results are seen: a conclusion is stated generally only if it holds for every main-comparison proxy in the main results and is not overturned in the common-cohort check; otherwise it is stated per construct.
- H1 and H2 are restated per construct (demography, land use) with predictions and the decision metric written down before Phase 3 runs (issue 1 in `Data/Plans/single_proxy_strategy_review_2026-10-23.md`).
- KK10 and HYDE count as one evidence family (shared population basis) and serve as a land-use scenario bracket, not as independent replicates.
- Shares are reported as variation associated with a representation, not as causal effects.
- Signed, zero-truncated, and exclude-negative profiles are all reported.
- One SPD scale (raw or square-root) and one residual-df minimum are used across canonical and sensitivity runs, or the difference is documented.
- No new temporal-model fit runs without an authorised row in `general_model_run_requests.csv`.
- Raw KK10 and HYDE rasters and target stores stay in configured external storage.

## Implementation phases

### Phase 0 - Repository health

- `R/analyses/00_profiles/pipeline_contracts.csv`: four profiles share `joint_human_proxy_hvarpart/pipeline.R`, which fails `validate_pipeline_contract_registry()` and makes `R/analyses/00_run_main.R` abort. Give each profile its own runner or allow shared scripts that differ by profile.
- `R/analyses/00_profiles/analysis_profiles.csv`: the KK10 and HYDE rows record the support as `not_applicable`, although extraction uses the SPD buffer.
- `R/analyses/03_temporal_models/01_inputs/prepare_data.R` reads the stale stores `pipeline_predictors` and `pipeline_paps`; repoint it to the `data_preparation/...` stores.
- Choose the SPD scale (canonical H1/H2 use raw SPD; all sensitivities use square-root SPD) and the residual-df minimum (5 canonical, 4 in sensitivities).
- Correct stale `source_pipeline` values in `Manuscript/COMMSENV-25-2408/R1/evidence/claim-evidence-registry.csv`.

Validation: `validate_pipeline_contract_registry()` and `validate_analysis_profiles()` pass, `00_run_main.R` passes validation, and the full test suite passes in a fresh subprocess.

### Phase 1 - Conceptual assessment and proxy independence checks

- Discuss `Data/Plans/human_proxy_conceptual_assessment_2026-10-23.md` and its stress test `Data/Plans/single_proxy_strategy_review_2026-10-23.md` with the co-authors, and resolve the "verify" items.
- KK10: the per-capita land-use assumption (high early land use) against HYDE (low early land use).
- HYDE 3.5: how prehistoric population and the IMSET and ArchaeoGLOBE onsets enter the product, its temporal steps before 1 CE, and whether early-Holocene land use is still close to zero. Repeat the SPD convergence analysis against HYDE 3.5, including first differences.
- KK10: the share of climate signal entering through GLUES before 3 ka (Krumhardt 2010; Kaplan et al. 2011 supplement) and whether the Bradshaw (2004) series is pollen-informed.
- KK10: whether any release extends beyond 8 ka.
- HYDE: the source of the pollen-circularity claim raised on 2026-10-22, and the coverage and steps of HYDE 3.3 and 3.5 against 3.2.
- ArchaeoGLOBE (optional, H2 only): coverage, resolution, access, and whether it can serve as a regional predictor or stratifier.
- Write the per-construct hypotheses (H1a/H2a demography versus climate; H1b/H2b land use versus climate), the decision metric, and the headline rule.

Validation: every cell of the construct table cites a source; G1 is recorded with the candidates retained and the hypotheses written down.

### Phase 2 - Canonical human-proxy data layer

Promote proxy extraction from `R/analyses/91_sensitivity_analyses/human_proxy_convergence/` into a new upstream `R/analyses/01_data_preparation/0X_human_proxies/` with contracted public targets.

- Extract at native coverage: KK10 8.0–0.5 ka, HYDE 11.7–0.5 ka, SPD to 2 ka. The current caches stop at 2–8 ka (`target_ages = seq(2000, 8000, 500)`).
- Extract the gridded proxies at a scale matched to the pollen-source area (the record's cell and small radii, with a sensitivity across radii), independent of the SPD buffer. SPD needs 250/500 km only because radiocarbon dates are sparse; the gridded proxies do not. Also extract every gridded proxy over the SPD buffer, for the convergence comparison and the scale-matched sensitivity (review, issue 6); the PR #343 extraction covers KK10 and HYDE 3.2 only, so HYDE 3.5 needs it anew.
- Add HYDE 3.5 (population, cropland, grazing; doi:10.24416/UU01-F45D44) as the HYDE candidate in place of 3.2 `popc`, with version, files, and checksums locked in the source manifest. Derive the land-use fraction (cropland plus grazing area over cell area).
- Flag every proxy value as an exact native layer or an interpolated value, for the native-step sensitivity (review, issue 7).
- Reuse `build_kk10_raster_cache()`, `build_human_proxy_raster_cache()`, `aggregate_human_proxy_raster()`, `interpolate_human_proxy_ages()`, `prepare_hyde_population_raster()`, and `validate_human_proxy_source_manifest()` from `R/functions/human_impact/comparison/`.
- Add a regional (region × climate zone) aggregation function for H2, because `aggregate_human_proxy_raster()` handles only buffers and points.

Validation: synthetic-raster tests for each support, unique dataset-age keys, no extrapolation, and coverage tables by source, region, and age that reconcile with the existing 8.0–2.0 ka matches.

### Phase 3 - Single-proxy H1 across proxies and windows

Replace the per-proxy sensitivity pipelines with one parameterised H1 under `R/analyses/02_h1_spatiotemporal_hvarpart/`, run once per human proxy.

- Drive the human proxy, window, and support from `analysis_profiles.csv` rows instead of the global set by the `run_*.R` scripts with `tar_make(callr_function = NULL)`.
- Build the shared climate selection as its own target per analytical unit (spatial H1: per record; temporal H1: per region × age slice), computed once and consumed by every proxy run. The existing collinearity-managed pipeline already does this for its SPD bridge (`build_human_proxy_model_specifications()`, `select_local_hvarpart_predictors()`, `validate_shared_local_climate_selections()`); generalise it from a bridge to the main design.
- Reuse `prepare_local_hvarpart_model_designs()`, `fit_local_temporal_hvarpart_models()`, `fit_local_spatial_hvarpart_models()`, and `fit_human_proxy_spatial_balance_models()`, and keep the canonical fitters.
- Proxy runs: SPD, KK10, HYDE 3.5 land use (main comparison); HYDE 3.5 population and expert events (supplementary). Each runs on the common cohort in the core window and on its full cohort in its native window, in its own store `analyses_h1/<profile_id>`.
- Report what climate reduction buys: eligible records and residual df with all four versus selected climate predictors, and climate retention frequencies by region.
- Add the human ∩ space diagnostic for temporal H1 (analysis G) and the human ∩ time diagnostic for spatial H1, for every proxy, with detrended proxy variants, and report each proxy's correlation with the selected climate variables per unit (review, issues 5 and 7).
- Supplementary overlap analysis (S3): fit demography and land use as separate focal groups (KK10 + SPD; KK10 + HYDE 3.5 population as a check) with the same shared climate selection, in both H1 scales, only where the residual-df minimum still holds. Export the commonality fractions (demography only, land use only, demography ∩ land use, and their overlaps with climate). `fit_varhp()` already calls the partitioning with `var.part = TRUE` (`R/functions/hvarpart/fitting/fit_varhp.R`), but the current `*_components.csv` exports keep only the unique, average-share, and individual columns.
- Write a memo for the co-authors as the successor to `human_proxy_hvarpart_decision_memo.qmd`, with live inline R.

#### Figure design

- **Temporal H1 (Figure 4):** one column per proxy (SPD, KK10, HYDE 3.5 land use) and one row per continental region, with the existing human/climate/space stacked bars through time. This extends the current SPD-versus-events layout of `plot_h1_temporal_controlled_composition()` and `plot_human_proxy_temporal_comparison()`. Main version: each proxy with its full data over its native window, so columns differ in time span and records, exactly as SPD and events do in the current figure. Supplementary version: the common cohort in the common 8.0–2.0 ka window, to show that the pattern across proxies does not come from sample composition. Events go in a supplementary column, visually flagged as circular.
- **Spatial H1 (Figure 3), new main figure in two panels:**
  - *Agreement map:* one point per record on the common cohort, coloured by how many of the three main proxies give a human-dominant balance (0 to 3). This shows directly where the conclusion is robust (0 or 3) and where it depends on the proxy (1 or 2).
  - *Regional forest plot:* the dbMEM-adjusted balance with its interval for each continental region (and overall), one point per proxy side by side, with sequence-level distributions as small ridgelines per proxy.
- **Spatial H1, supplementary:** the full canonical Figure 3 layout (map, density, climate-zone panels) repeated for each proxy, using `plot_h1_spatial_controlled_balance()` / `plot_human_proxy_spatial_balance_comparison()` unchanged.
- **Overlap (S3), supplementary:** per continental region, stacked fractions of demography only, land use only, and demography ∩ land use (plus their overlaps with climate), for spatial H1 (median across records) and temporal H1 (through time).
- New plotting functions: the agreement map, the regional forest plot, and the overlap figure. All read the exported record, aggregation, and commonality tables of the proxy runs.

Validation: explicit fit statuses with no silent errors, identical climate selections across proxy runs within each unit, identical cohorts in the main comparison, plotted values reconstructed from exported tables, and G2 recorded.

### Phase 4 - Full H2 refit for each human proxy

- Keep H2 predictors as brms trend predictions per region × climate zone: add KK10 and HYDE 3.5 land use to `prepare_predictor_model_data()` and `predictor_temporal`, and add model families (land-use fractions: zero-one-inflated beta; HYDE population, if used: hurdle-gamma or square-root).
- Generalise `prepare_h2_hvarpart_data()` and the hard-coded `predictor_vars` in `R/analyses/04_h2_multidimensional_shifts/pipeline.R`, and run H2 once per proxy with one human predictor and the same climate selection rule as H1.
- Before G2, run an exploratory H2 preview from region × climate-zone means of the Phase 2 proxies, labelled as mixing smoothed and unsmoothed predictors.
- Make `output_hvar_h2_*` a public contracted target so `R/analyses/05_visualisations/h2/` stops reading a private target.

Validation: new brms fits run only from authorised ledger rows, model diagnostics are exported, and H2 allocations are reported for every proxy on the same groups.

### Phase 5 - Reviewer-driven validation for each proxy

All R1 checks were run with SPD and all four climate predictors. Rerun them for the three main-comparison proxies with the shared climate selection; the H1 fits are cheap compared with the brms models, so the whole suite can run per proxy:

- SPD radius 250 versus 500 km (SPD only), and extraction-radius sensitivity for KK10 and HYDE 3.5.
- Spatial thinning and leave-one-group-out.
- Residual Moran diagnostics.
- Reduced-PAP response set.
- Signed, zero-truncated, and exclude-negative profiles.
- Climate-representation sensitivity: all four climate variables (where residual df allow), the filtered selection, and climate principal components; report how much the balance moves (review, issue 3).
- Scale-matched sensitivity: KK10 and HYDE 3.5 averaged over the SPD buffer, from the Phase 2 extraction (review, issue 6).
- Native-step sensitivity: only ages with exact proxy layers, so no interpolated values (review, issue 7).
- Transformation sensitivity: rank or log-type transforms for every proxy (review, issue 8).
- If feasible, SPD resampling sensitivity with bootstrapped date sets for one scale (review, issue 9).
- Expert events as a supplementary, visually flagged sensitivity (as in R1), including expert-confirmed zeros rather than only records with events.
- Circularity-reduced event sensitivity: recompute PAPs without the event indicator taxa before using events as predictors.
- Total variance explained (editor's request).
- #341: H1 refit with at least 300 counted grains, in separate stores.
- Optional, if agreed: analysis D, single PAP versus whole assemblage (`response_vars_h1`).

Validation: each check exports its comparison table and figure from its own store, and every conclusion statement reconciles with those tables.

### Phase 6 - Manuscript and reviewer response

Start after G3.

- Restructure `Manuscript/COMMSENV-25-2408/R1/_sections/` around the multi-proxy design and framing.
- Add registry rows for the proxy data layer, the proxy runs, H2, and validation outputs; the registry currently has none for KK10 or HYDE.
- Rewrite the title, abstract, Results, Discussion, and Methods (human proxies and their constructs, KK10 and HYDE as one evidence family, per-construct hypotheses, single-proxy design with shared response-free climate selection, headline rule, association language, window rationale including the 2 ka SPD limit).
- Fold `R1/Reply/response_spd_proxy_convergence.qmd` and `spd_proxy_convergence_manuscript_patch.md` into Methods and SI with live numbers, and close #342.
- Rewrite the reviewer letters rather than patching them.

Validation: manuscript and reply render, revision assets assemble, and every numerical statement is an inline R expression traced to the registry.

## Order of work

Phase 0 and Phase 1 run in parallel, then G1 (hypotheses fixed), then Phase 2, then Phase 3 with the H2 preview, then G2.
Phase 4 and Phase 5 then run in parallel, followed by G3 and Phase 6.

## Reproducibility boundaries

- Do not overwrite canonical R1 stores, outputs, or temporal-model fits.
- Do not commit raw rasters or target state.
- Add no package dependency without a scoped `renv` change.
- Leave repository changes uncommitted and unpushed for human review.

## Issues

The GitHub issue plan, with updates to #317, #323, #341, and #342 and ready-to-paste bodies for the new issues, is in `Data/Plans/issue_plan_human_impact_reframing_2026-10-23.md`. Nothing has been created on GitHub.
