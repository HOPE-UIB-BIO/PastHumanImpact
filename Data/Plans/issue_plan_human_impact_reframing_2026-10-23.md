# Issue plan: human-impact reframing

Status: created on GitHub on 2026-10-23. New issues: A = #344, B = #345, C = #346, D = #347, E = #348, F = #349, G = #350, H = #351, I = #352, J = #353, K = #354, all sub-issues of #317 in the milestone "🐦‍🔥Resubmit the paper". #317 body replaced; update sections appended to #323, #341, and #342. The optional single-PAP issue was not created. The bodies below keep the letter placeholders; GitHub holds the filled versions.
Based on the GitHub state on 2026-10-23 and written to `C:\Users\ondre\.claude\Git_workflow.md` section 7: every body is self-contained (context with numbers, definitions and thresholds in full, code paths and targets checked against the code on 2026-10-23), uses the layout Context, Goal, Scope, Tests and validation, Acceptance criteria, Relationships, and does not depend on any planning file.

## How to use this file

- New issues are referred to by letter (A–K) until they have numbers. Create them in letter order, which is dependency order, and replace each `#[X]` placeholder with the real number before creating the next one. Then back-fill the #317 checklist and link every new issue as a sub-issue of #317.
- All new issues: milestone "🐦‍🔥Resubmit the paper" (existing). Labels are taken from the existing label set only.
- Paths marked "(new)" do not exist yet and are proposals. Every other path, function, and target was checked in the code.
- Changes to existing issues (#317, #323, #341, #342) need your confirmation before they are applied.

## Current GitHub state

Open, all in the milestone "🐦‍🔥Resubmit the paper": #317 (umbrella), #323 (manuscript), #341 (300-grain rerun), #342 (SPD versus KK10 and HYDE).
Closed sub-issues of #317: #318, #319, #321, #322, #325, #326, #327, #329, #333.

## Overview

| Ref | Title | Labels | Blocked by |
| --- | --- | --- | --- |
| A | Restore pipeline registry and provenance consistency before adding human proxies | 🐛 bug, 🔨Pipeline refactor, 💻code | — |
| B | Define human-impact constructs, proxy set, and pre-specified hypotheses | 📈⚙️methodology progress, 📝method documentation, 🗣️communication, Not decided | — |
| C | Build a canonical human-proxy data layer at native temporal coverage | 🗂️data, 💻code, 🔨Pipeline refactor | A, B |
| D | Fit spatial and temporal H1 once per human proxy with a shared climate selection | H1, 📈⚙️methodology progress, 💻code | B, C |
| E | Diagnose whether gridded-proxy human shares reflect smooth structure or climate | H1, 📈⚙️methodology progress, 💻code | D |
| F | Quantify the overlap between demography and land-use representations of human impact | H1, 📈⚙️methodology progress, 💻code | D |
| G | Plot H1 results across human proxies | H1, 🖼️visualisation, 💻code | D, F |
| H | Report single-proxy H1 evidence to the co-authors and record the design decision | 🗣️communication, 📝method documentation | D, E, F, G |
| I | Refit H2 once per human proxy | H2, 📈⚙️methodology progress, 💻code | C, H |
| J | Rerun the R1 robustness checks for each human proxy | H1, 📈⚙️methodology progress, 💻code | D, H |
| K | Test the single-proxy design against reviewer-style objections | H1, 📈⚙️methodology progress, 💻code | D, H |
| Optional | Compare single-PAP and whole-assemblage attribution across human proxies | H1, Not decided | D |

Existing issues: #342 is updated to add HYDE 3.5 (blocked by C); #341 is rescoped per proxy (blocked by D, H); #323 is updated for the rewrite (blocked by H, I, J, K, #341); #317 gets a new body.

Order of work: A and B in parallel; C; D; E, F, G; H; I, J, K, and #341 in parallel; the publication-route decision in #317; #323.

## Shared definitions used in the bodies

These definitions are repeated inside each issue that needs them, so that every issue stands alone. They are listed once here for checking.

- **PAPs:** ten pollen assemblage properties, `response_vars_h1` in `R/analyses/02_h1_spatiotemporal_hvarpart/01_inputs/pipeline.R`: `n0`, `n1`, `n2`, `n1_minus_n2`, `n2_divided_by_n1`, `n1_divided_by_n0`, `roc`, `dcca_axis_1`, `density_diversity`, `density_turnover`.
- **Climate predictors:** `temp_annual`, `temp_cold`, `prec_summer`, `prec_win` (`predictor_sets_h1` in the same file).
- **HVarPart:** hierarchical variation partitioning of the multivariate PAP response by distance-based RDA (`fit_varhp()`, `R/functions/hvarpart/fitting/fit_varhp.R`).
- **Spatial H1:** one model per pollen record through its 500-year ages, with time as a protected control group (`R/analyses/02_h1_spatiotemporal_hvarpart/03_time_control/`); record balances are aggregated with dbMEM spatial control (`05_spatial_aggregation/`). Current Figure 3.
- **Temporal H1:** one model per continental region × 500-year age slice across records, with selected dbMEMs as a spatial control group (`04_spatial_control/`). Current Figure 4.
- **Eligibility** (`h1_analysis_config` in the H1 input pipeline): spatial H1 needs at least 10 unique ages and 5 residual degrees of freedom; dbMEM models need at least 20 unique locations and 10 residual df; 999 permutations; dbMEM distances 250 and 500 km. The PR #343 sensitivity pipelines relaxed the temporal residual-df minimum to 4.
- **Zero-truncated balance** (column `zero_balance` in the spatial-balance record tables): negative components set to zero, then (human − climate) / (human + climate), from −1 (all climate) to +1 (all human). A record is human-dominant when `zero_balance > 0`. The signed balance (`signed_balance`) is (human − climate) / total adjusted R².
- **Temporal allocation** (column `allocation` in the spatial-control composition tables): each group's zero-truncated individual contribution divided by the zero-truncated total of human, climate, and space in that region × age unit. A unit is human-dominant when the human allocation exceeds the climate allocation.
- **Human proxies:**
  - SPD: summed calibrated radiocarbon-date probabilities within 250 km of each record, falling back to 500 km when fewer than 50 dates fall within 250 km (`data_spd_250_with_500_fallback`, `R/analyses/01_data_preparation/03_archaeological_proxies/spd/pipeline.R`, `minimum_dates = 50L`). Represents relative human population (demography), not land use. Unreliable after 2 ka BP because radiocarbon dating declines once written sources exist; models with SPD stop at 2 ka.
  - KK10: modelled anthropogenic land-use fraction (Kaplan et al. 2011; PANGAEA doi:10.1594/PANGAEA.871369), 5' grid, 8 ka to AD 1850. Built from regional population (historical estimates; the climate-driven GLUES model before about 3 ka), a per-capita land-use curve that gives high early land use, and allocation by present-day land suitability.
  - HYDE 3.5: Utrecht University and PBL Netherlands Environmental Assessment Agency (2025), doi:10.24416/UU01-F45D44, 5' grid, 10,000 BCE to 2025 CE, population count plus cropland and grazing area. Agricultural onset dates come from IMSET radiocarbon data in Eurasia and ArchaeoGLOBE elsewhere. PR #343 used HYDE 3.2 population (`popc.tif`, doi:10.7910/DVN/E3H3AK).
  - Expert events: pollen-based human-impact stages assessed per record (Europe `fi`, `fc`, `ec`, `cc`; North America `fc`, `es`; Asia `fi`, `fc`, `ei`; Latin America weak, strong; Oceania weak, medium, strong). Every record was assessed; zeros are expert-confirmed absences. Circular, because events and PAPs derive from the same pollen.
- **Single-proxy design:** each model contains exactly one human predictor. Climate predictors are selected once per analytical unit without using the response, with `collinear` 3.0.2 (`max_cor = 0.8`, `max_vif = 5`, preference order `temp_annual`, `temp_cold`, `prec_summer`, `prec_win`, at least one retained), and the same selection is used by every proxy's model in that unit. Time and dbMEM controls are protected and added after selection.
- **Main-comparison proxies:** SPD, KK10, HYDE 3.5 land use. Supplementary: HYDE 3.5 population, expert events.
- **Native windows** (500-year ages): SPD 2,000–11,500 BP; KK10 500–8,000 BP; HYDE 3.5 500–11,500 BP. **Common cohort:** units with all three main proxies available, ages 2,000–8,000 BP.
- **Headline rule:** a conclusion is stated generally only if it holds for every main-comparison proxy in the main results and is not overturned in the common-cohort check; otherwise it is stated per construct.
- **Exploratory PR #343 results (8,000–2,000 BP, all four climate predictors unless noted):** spatial H1 human-dominant records 18.9 % for canonical SPD (8,500–2,000 BP, overall dbMEM-adjusted zero-truncated balance −0.466), 28.5 % for SPD with the reduced shared climate set, 55.1 % for KK10 (+0.036), 61.6 % for HYDE 3.2 (+0.101); temporal H1 human-dominant units out of 65: 1, 0, 0, and 2.

## Updates to existing issues (confirm before applying)

### #317 — replace the body

```markdown
## Context
The SPD-based R1 analyses are complete (#318, #319, #321, #322, #325, #326, #327, #329, #333). PR #343 then showed that the H1 attribution depends on how human impact is represented. Over 8,000–2,000 BP, the share of pollen records whose PAP variation is allocated more to humans than to climate (spatial H1) is 18.9 % with the canonical SPD, 55.1 % with KK10 land use, and 61.6 % with HYDE 3.2 population, while temporal H1 stays climate-dominant for every proxy (0–2 of 65 region × age units human-dominant). SPD represents human population, not land use, so "climate outweighs human impact" was a claim about demography only.

## Goal
Deliver the resubmission with analyses that represent human impact by several proxies, one human predictor per model, so that conclusions are stated for each construct (demography, land use) and generalised only when they hold for all main proxies; plus revised figures, a rewritten manuscript, and a rewritten reviewer response.

## Scope
- [ ] #[A] Restore pipeline registry and provenance consistency
- [ ] #[B] Define human-impact constructs, proxy set, and pre-specified hypotheses
- [ ] #342 Validate SPDs against KK10 and HYDE (HYDE 3.5 added)
- [ ] #[C] Build a canonical human-proxy data layer
- [ ] #[D] Fit H1 once per human proxy
- [ ] #[E] Diagnose gridded-proxy human shares
- [ ] #[F] Quantify demography versus land-use overlap
- [ ] #[G] Plot H1 results across human proxies
- [ ] #[H] Report single-proxy evidence and record the design decision
- [ ] #[I] Refit H2 once per human proxy
- [ ] #[J] Rerun the R1 robustness checks per proxy
- [ ] #[K] Test the design against reviewer-style objections
- [ ] #341 300-grain refit per proxy
- [ ] #323 Manuscript and reviewer response

## Decision points
- [ ] Proxies retained, per-construct hypotheses, decision metric, and headline rule written down before any new model run (#[B]).
- [ ] Single-proxy design, proxy set, windows, and framing confirmed from the evidence (#[H]).
- [ ] Publication route: resubmit as R1 of COMMSENV-25-2408 or as a new manuscript, decided before #323 starts.

## Tests and validation
- [ ] Each sub-issue passes its own acceptance criteria.

## Acceptance criteria
- [ ] All sub-issues closed.
- [ ] Every manuscript number is an inline R expression traced to a registered output in `Manuscript/COMMSENV-25-2408/R1/evidence/claim-evidence-registry.csv`.
- [ ] Revision package artifacts present in project outputs and manuscript files.

## Relationships
- Part of milestone "🐦‍🔥Resubmit the paper"
```

### #342 — add an update section, keep open

```markdown
## Update (2026-10-23)

### Context
PR #343 delivered the convergence analysis: matched SPD, KK10, and HYDE 3.2 values for 1,181 pollen records over 2,000–8,000 BP (`data_human_proxy_matches` in store `sensitivity_analyses/human_proxy_convergence`; columns `dataset_id`, `age_bp`, `spd`, `radius_km`, `kk10`, `hyde`, `region`, `long`, `lat`, `spd_transformed`, `kk10_transformed`, `hyde_transformed`). Kendall tau between SPD-decile bin medians is 0.87 (bootstrap 0.64–0.91) for KK10 and 0.69 (0.56–0.78) for HYDE 3.2, but only 0.07 for both on first differences (`Outputs/Tables/H1/Sensitivity/Human_proxy_convergence/correlations.csv`, `trend_sensitivity.csv`). HYDE 3.5 (doi:10.24416/UU01-F45D44) replaces HYDE 3.2 as the HYDE candidate; it adds radiocarbon-based (IMSET) and ArchaeoGLOBE agricultural onset dates.

### Scope
- [ ] Repeat the convergence analysis for SPD against HYDE 3.5 population and HYDE 3.5 land-use fraction, with the same SPD-decile binning, Kendall tau, dataset-clustered bootstrap, and first-difference sensitivity, using the SPD-buffer extraction from #[C].
- [ ] Report whether HYDE 3.5 agrees with SPD only in level or also in change.
- [ ] During the manuscript rewrite (#323), move the interpretation into Methods and SI with inline values, replacing the hand-typed numbers in `Manuscript/COMMSENV-25-2408/R1/Reply/spd_proxy_convergence_manuscript_patch.md`.

### Acceptance criteria
- [ ] HYDE 3.5 correlations can be recomputed from the exported bin table.
- [ ] Manuscript statements match the exported tables.

### Relationships
- Blocked by #[C]
- Part of #317
```

### #341 — add an update section

```markdown
## Update (2026-10-23)

### Context
The H1 analyses now run once per human proxy (SPD, KK10 land use, HYDE 3.5 land use), each model with one human predictor and a climate set selected once per unit without the response (`collinear`, `max_cor = 0.8`, `max_vif = 5`) and shared by all proxies. The 300-grain rerun must follow the same design so that its comparison is valid for every proxy.

### Scope
- [ ] Run the at-least-300-grain filter through the parameterised H1 pipeline from #[D] for the three main proxies, in separate stores.
- [ ] Compare baseline and 300-grain results per proxy: share of human-dominant records (`zero_balance > 0`), overall dbMEM-adjusted balance, human-dominant region × age units, and whether the headline-rule outcome changes.

### Acceptance criteria
- [ ] Baseline stores are not overwritten.
- [ ] Retention totals reconcile with the filtered inputs.

### Relationships
- Blocked by #[D]
- Blocked by #[H]
- Part of #317
```

### #323 — add an update section

```markdown
## Update (2026-10-23)

### Context
The R1 text, title ("Climate outweighs human effects on pollen assemblage dynamics from 8.5 to 2.0 ka BP"), and reviewer letters rest on SPD-only results. With several human proxies giving different spatial results, the manuscript and the letters are rewritten, not patched.

### Scope
- [ ] Restructure `Manuscript/COMMSENV-25-2408/R1/_sections/` around per-construct hypotheses (demography versus climate; land use versus climate) and results per human proxy.
- [ ] Add registry rows to `Manuscript/COMMSENV-25-2408/R1/evidence/claim-evidence-registry.csv` for the proxy data layer, proxy runs, H2, and validation outputs (no KK10 or HYDE rows exist yet).
- [ ] Methods: human proxies and what each represents; KK10 and HYDE as one evidence family (shared population basis); one human predictor per model; shared response-free climate selection; headline rule; association rather than causal language; SPD stops at 2 ka because radiocarbon dating declines once written sources exist.
- [ ] Rewrite the title, abstract, Results, Discussion, and reviewer letters.

### Acceptance criteria
- [ ] Every numerical statement is inline R traced to the registry.
- [ ] Manuscript and reply render; revision assets assemble.

### Relationships
- Blocked by #[H]
- Blocked by #[I]
- Blocked by #[J]
- Blocked by #[K]
- Blocked by #341
- Part of #317
```

## New issues

### A. Restore pipeline registry and provenance consistency before adding human proxies

Labels: 🐛 bug, 🔨Pipeline refactor, 💻code

```markdown
## Context
`R/analyses/00_run_main.R` aborts at contract validation. `validate_pipeline_contract_registry()` (`R/functions/workflow/configuration/validate_pipeline_contract_registry.R`) requires every contract row to own a unique script, but four rows in `R/analyses/00_profiles/pipeline_contracts.csv` (`joint_human_proxy_hvarpart`, `kk10_only_hvarpart`, `hyde_only_hvarpart`, `spd_kk10_hvarpart`) point to `R/analyses/91_sensitivity_analyses/joint_human_proxy_hvarpart/pipeline.R`. The profile is chosen by a global variable (`human_proxy_hvarpart_profile`) set in `run_kk10.R`, `run_hyde.R`, and `run_spd_kk10.R` in the same folder.

Further inconsistencies:
- `R/analyses/00_profiles/analysis_profiles.csv` gives `spd_radius_specification = not_applicable` for the KK10 and HYDE rows (`within_dataset_kk10_time_control`, `time_slice_kk10_spatial_control`, `within_dataset_hyde_time_control`, `time_slice_hyde_spatial_control`), although their values were averaged over the SPD buffer (250 km, 500 km fallback).
- `R/analyses/03_temporal_models/01_inputs/prepare_data.R` (lines 41 and 50) reads the stores `Targets_data/pipeline_predictors` and `Targets_data/pipeline_paps`; the current stores are `data_preparation/predictors` and `data_preparation/paps`.
- Canonical H1 and H2 use raw SPD (column `spd`), while all PR #343 sensitivity pipelines use square-root SPD (`spd_transformed`).
- `h1_analysis_config` sets `min_temporal_residual_df = 5L`; the sensitivity pipelines use 4.
- In `Manuscript/COMMSENV-25-2408/R1/evidence/claim-evidence-registry.csv`, eight `source_pipeline` values do not exist: `01_data_preparation/11_figures/pipeline.R`, `01_data_preparation/pollen_thresholds/pipeline.R`, `03_temporal_models/07_figures/pipeline.R`, `03_temporal_models/07_summaries/pipeline.R`, `05_h2_interrelationships/pipeline.R`, `90_diagnostics/hvarpart/pipeline.R`, `90_diagnostics/spatiotemporal_dependence/pipeline.R`, `91_sensitivity_analyses/pap_collinearity/pipeline.R` (paths relative to `R/analyses/`); six others lack the `R/analyses/` prefix.

## Goal
Make the registries, profiles, and stores consistent so that new human-proxy pipelines can be added and the full workflow validates.

## Scope
- [ ] Give each human-proxy profile its own runner script, or extend the contract validator to allow one script shared by profiles that differ only by a declared profile argument; remove the global-variable mechanism.
- [ ] Correct the KK10 and HYDE rows in `analysis_profiles.csv` to record the support actually used.
- [ ] Repoint `prepare_data.R` to the `data_preparation/predictors` and `data_preparation/paps` stores via `resolve_pipeline_store_path()`.
- [ ] Decide one SPD scale (raw or square-root) and one temporal residual-df minimum (4 or 5) for all new runs, and record the decision in this issue.
- [ ] Correct or remove the stale `source_pipeline` values and add the `R/analyses/` prefix where missing.

## Tests and validation
- [ ] Unit tests for the contract validator cover the chosen rule for shared scripts.
- [ ] Smoke-run `R/analyses/00_run_main.R` to the end of validation.

## Acceptance criteria
- [ ] `validate_pipeline_contract_registry()` and `validate_analysis_profiles()` pass.
- [ ] Every `source_pipeline` in the registry exists.
- [ ] The SPD-scale and residual-df decisions are recorded here.
- [ ] The full test suite passes in a fresh subprocess (`run_project_tests()`).

## Relationships
- Part of #317
```

### B. Define human-impact constructs, proxy set, and pre-specified hypotheses

Labels: 📈⚙️methodology progress, 📝method documentation, 🗣️communication, Not decided

```markdown
## Context
The R1 hypotheses ask whether human impact is the predominant predictor of PAP change, but the human predictor was an SPD of archaeological radiocarbon dates, which represents relative human population (demography), not what people did to vegetation. PR #343 showed the answer depends on the proxy: over 8,000–2,000 BP, the share of records whose PAP variation is allocated more to humans than to climate is 18.9 % with canonical SPD, 28.5 % with SPD and a reduced climate set, 55.1 % with KK10 land use, and 61.6 % with HYDE 3.2 population; temporal H1 is climate-dominant for every proxy (0–2 of 65 region × age units).

Human influence runs from presence (population) to land use to realised vegetation impact to the pollen signal. The closer a proxy is to realised impact, the less independent it is of the pollen response:

| Proxy | Construct | Independent of pollen | Independent of climate | Coverage |
| --- | --- | --- | --- | --- |
| SPD (250 km, 500 km fallback) | demography, empirical | yes | yes | about 12–2 ka; unreliable after 2 ka |
| KK10 (doi:10.1594/PANGAEA.871369) | land use, modelled, high early land use | probably; check the Bradshaw (2004) calibration series | check: GLUES population before 3 ka is climate-driven; allocation uses present-day suitability | 8 ka–AD 1850 |
| HYDE 3.5 (doi:10.24416/UU01-F45D44) | population and land use, modelled, low early land use | yes (3.2 confirmed) | land-use allocation uses BIOME and NPP weights; population does not | 10,000 BCE–2025 CE; millennial steps before 1 CE in 3.2 |
| Expert events | realised local impact | no (circular) | n/a | full Holocene |

KK10 and HYDE share their population basis (McEvedy and Jones type estimates; within-record r = 0.96 between KK10 and HYDE 3.2), so they form one evidence family and differ mainly in land-use assumptions.

The proposed design fits one human predictor per model. With several proxies and two H1 scales, any pattern could be presented as a result unless hypotheses and a decision rule are fixed before the runs.

## Goal
Agree with the co-authors which constructs and proxies represent human impact, and write down the hypotheses, decision metric, and headline rule before any new model is run.

## Scope
- [ ] Resolve open facts: KK10 per-capita land-use assumption and the share of GLUES climate input before 3 ka (Kaplan et al. 2011 supplement; Krumhardt 2010); whether the Bradshaw (2004) forest-cover series is pollen-informed; whether any KK10 release extends beyond 8 ka; HYDE 3.5 population method, time steps before 1 CE, and early-Holocene land use (HYDE 3.2 is close to zero before about 3 ka); the source of the claim, raised on 2026-10-22, that HYDE uses pollen (not supported for HYDE 3.2).
- [ ] Optional: assess ArchaeoGLOBE (expert land-use stages for about 146 regions, about 10 ka–AD 1850) as a regional H2 predictor or stratifier.
- [ ] Confirm the proxy set. Proposed main comparison: SPD (demography), KK10 land-use fraction (land use, high early use), HYDE 3.5 land-use fraction (land use, low early use). Proposed supplementary: HYDE 3.5 population (check on SPD) and expert events (flagged as circular).
- [ ] Write the hypotheses. Proposed: H1a and H2a, demography explains more PAP variation than climate; H1b and H2b, land use explains more PAP variation than climate; each tested at both H1 scales.
- [ ] Write the decision metric. Proposed: spatial H1, sign and interval of the overall dbMEM-adjusted zero-truncated balance, where the record balance is (human − climate) / (human + climate) after setting negative components to zero; temporal H1, number of region × age units where the human allocation exceeds the climate allocation.
- [ ] Write the headline rule. Proposed: a conclusion is stated generally only if it holds for every main-comparison proxy in the main results (each proxy's full data over its native window) and is not overturned in the common-cohort check (units with all main proxies, 2,000–8,000 BP); otherwise it is stated per construct.

## Tests and validation
- [ ] Every cell of the construct table above cites a source.

## Acceptance criteria
- [ ] Proxy set, hypotheses, decision metric, and headline rule recorded in this issue and agreed by the co-authors before #[D] runs.
- [ ] Open facts resolved or explicitly marked as limitations.

## Relationships
- Part of #317
```

### C. Build a canonical human-proxy data layer at native temporal coverage

Labels: 🗂️data, 💻code, 🔨Pipeline refactor

```markdown
## Context
KK10 and HYDE are extracted only inside the sensitivity pipeline `R/analyses/91_sensitivity_analyses/human_proxy_convergence/pipeline.R` (store `sensitivity_analyses/human_proxy_convergence`; public targets include `data_human_proxy_kk10_primary`, `data_human_proxy_hyde_primary`, `data_human_proxy_matches`). Three limits block the new analyses:
- Ages stop at 2,000–8,000 BP (`target_ages = seq(2000, 8000, by = 500)`, line 78), but KK10 covers 8 ka to AD 1850 and HYDE 10,000 BCE to 2025 CE.
- KK10 and HYDE are averaged over the SPD buffer (250 km, 500 km fallback). The SPD needs that buffer only because radiocarbon dates are sparse (fallback when fewer than 50 dates fall within 250 km); the 5' grids (about 9 km) can be extracted at the scale of the pollen-source area (tens of km).
- HYDE is version 3.2 population (`popc.tif`). HYDE 3.5 (Utrecht University and PBL 2025, doi:10.24416/UU01-F45D44) adds cropland and grazing layers and archaeologically anchored agricultural onsets.

## Goal
Provide SPD, KK10, and HYDE 3.5 as contracted upstream targets at each proxy's native temporal coverage, with gridded proxies extracted at the pollen-source scale and, for sensitivities, over the SPD buffer.

## Scope
- [ ] Create `R/analyses/01_data_preparation/07_human_proxies/pipeline.R` (new; store `data_preparation/human_proxies`, new) and register it in `pipeline_contracts.csv` and `R/analyses/01_data_preparation/00_run.R`.
- [ ] Reuse `build_kk10_raster_cache()`, `build_human_proxy_raster_cache()`, `aggregate_human_proxy_raster()`, `interpolate_human_proxy_ages()`, `prepare_hyde_population_raster()`, and `validate_human_proxy_source_manifest()` from `R/functions/human_impact/comparison/`.
- [ ] Add HYDE 3.5 population, cropland, and grazing to the source manifest with version, file names, sizes, and checksums; derive the land-use fraction as (cropland + grazing area) / cell area.
- [ ] Extract at 500-year ages: KK10 500–8,000 BP; HYDE 3.5 500–11,500 BP; SPD as now (to 2,000 BP). No extrapolation; the 0 BP age is excluded for KK10.
- [ ] Supports: the record's grid cell and small radii (proposed 10, 25, 50 km), plus the SPD buffer of each record.
- [ ] Add a logical column marking whether each value is an exact native layer or interpolated.
- [ ] Add a function aggregating proxies to continental region × climate zone × age for H2 (`aggregate_human_proxy_raster()` handles only buffers and points).
- [ ] Proposed public targets: `data_human_proxy_values` (columns `dataset_id`, `age_bp`, `proxy`, `support`, `radius_km`, `value`, `value_transformed`, `is_native_layer`) and `data_human_proxy_regional` (columns `region`, `climatezone`, `age_bp`, `proxy`, `value`); plus coverage and provenance tables.

## Tests and validation
- [ ] testthat tests with synthetic rasters for every support (cell, radius, SPD buffer) with hand-checkable means.
- [ ] Tests for unique `dataset_id × age_bp × proxy × support` keys, no extrapolation, and the native-layer flag.
- [ ] Smoke-run the pipeline on a subset of records.

## Acceptance criteria
- [ ] Values at the SPD buffer for KK10 over 2,000–8,000 BP reproduce `data_human_proxy_kk10_primary` within numerical tolerance.
- [ ] Coverage tables by proxy, region, and age are exported to `Outputs/Tables/Data/Human_proxies/` (new).
- [ ] Targets registered as public in `pipeline_contracts.csv`; validators pass.

## Relationships
- Blocked by #[A]
- Blocked by #[B]
- Part of #317
```

### D. Fit spatial and temporal H1 once per human proxy with a shared climate selection

Labels: H1, 📈⚙️methodology progress, 💻code

```markdown
## Context
PR #343 fitted joint human blocks (SPD + KK10, SPD + KK10 + HYDE). These are not pursued: spatial-H1 models over 8,000–2,000 BP have 13 ages per record, every extra predictor costs a residual degree of freedom (the unfiltered three-proxy model lost 272 records to insufficient residual df), a larger human block gains capacity to take shared variance, and pooled proxies mix demography with land use.

Instead, each model contains exactly one human predictor. Climate (`temp_annual`, `temp_cold`, `prec_summer`, `prec_win`) is reduced once per analytical unit without using the response, with `collinear` 3.0.2 (`max_cor = 0.8`, `max_vif = 5`, preference in that order, at least one retained), and the same selection is used by every proxy in that unit, so runs differ only in the human representation. The PR #343 collinearity-managed pipeline already does this for its SPD comparison (`R/analyses/91_sensitivity_analyses/human_proxy_hvarpart_collinearity_managed/pipeline.R`, store `sensitivity_analyses/human_proxy_hvarpart_collinearity_managed`, output `shared_climate_selection.csv`).

Definitions:
- Spatial H1: one HVarPart model per pollen record through its 500-year ages (ten PAPs: `n0`, `n1`, `n2`, `n1_minus_n2`, `n2_divided_by_n1`, `n1_divided_by_n0`, `roc`, `dcca_axis_1`, `density_diversity`, `density_turnover`), time as a protected control group, eligibility at least 10 unique ages and the residual-df minimum from #[A]; record balances aggregated with dbMEM spatial control.
- Temporal H1: one HVarPart model per continental region × 500-year age slice across records, selected dbMEMs (250 and 500 km) as a spatial control group, at least 20 unique locations and 10 residual df.
- Zero-truncated record balance: (human − climate) / (human + climate) after setting negative components to zero.

Exploratory PR #343 results (8,000–2,000 BP): human-dominant records 28.5 % (SPD, reduced climate), 55.1 % (KK10), 61.6 % (HYDE 3.2); human-dominant temporal units 0, 0, and 2 of 65.

## Goal
Fit spatial and temporal H1 once per human proxy through one parameterised pipeline, on each proxy's full data over its native window and on a common cohort.

## Scope
- [ ] Extend `R/analyses/02_h1_spatiotemporal_hvarpart/` so that the human proxy, window, and support come from `analysis_profiles.csv` rows (new rows per proxy and scale), replacing `predictor_sets_h1` as the only source of predictor sets.
- [ ] Add a shared climate-selection target per analytical unit (spatial H1: per record; temporal H1: per region × age), generalising `build_human_proxy_model_specifications()`, `select_local_hvarpart_predictors()`, and `validate_shared_local_climate_selections()`.
- [ ] Fit with `prepare_local_hvarpart_model_designs()`, `fit_local_temporal_hvarpart_models()`, `fit_local_spatial_hvarpart_models()`, and `fit_human_proxy_spatial_balance_models()`; keep `fit_temporal_hvarpart_datasets()`, `fit_spatial_importance()`, and `fit_spatial_hvarpart_dataset()` as the canonical fitters.
- [ ] Runs, each in its own store `analyses_h1/<profile_id>` (new): SPD, KK10 land use, HYDE 3.5 land use (main); HYDE 3.5 population and expert events (supplementary). Windows: native (SPD 2,000–11,500 BP; KK10 500–8,000 BP; HYDE 3.5 500–11,500 BP) on each proxy's full data, and 2,000–8,000 BP on the common cohort of units with all three main proxies.
- [ ] Export per run: record balances (same columns as `spd_kk10__spatial_balance_records.csv`, including `zero_balance`, `signed_balance`, `zero_human`, `zero_climate`, `zero_time`, `status`), aggregated estimates (`aggregation_level`, `profile`, `adjusted_balance`, `n_records`, `region`, `climatezone`), temporal compositions (`analysis`, `region`, `age`, `predictor`, `allocation`, `status`), and the climate selection per unit, to `Outputs/Tables/H1/Proxies/<profile_id>/` (new).
- [ ] Report eligible records and residual df with all four versus the selected climate predictors, and climate retention frequency by region.

## Tests and validation
- [ ] Tests that every proxy run in a unit uses an identical climate selection.
- [ ] Tests that common-cohort runs share identical units.
- [ ] Smoke-run one region per scale before full runs.

## Acceptance criteria
- [ ] Every model has an explicit status; no silent errors.
- [ ] Restricted to the PR #343 comparison cohort (1,069 records; `model_id == "spd_matched_bridge"` in `Outputs/Tables/H1/Sensitivity/Human_proxy_hvarpart_collinearity_managed/decision_spatial_common_cohort.csv`), the SPD run on 2,000–8,000 BP with the shared climate set reproduces the PR #343 result (305 of 1,069 records human-dominant, 28.5 %) within the expected tolerance.
- [ ] All tables exported for every run and window.

## Relationships
- Blocked by #[B]
- Blocked by #[C]
- Part of #317
```

### E. Diagnose whether gridded-proxy human shares reflect smooth structure or climate

Labels: H1, 📈⚙️methodology progress, 💻code

```markdown
## Context
When KK10 enters temporal H1 (models per continental region × 500-year age slice with dbMEM spatial control), the median space allocation falls from 0.255 to about 0.03 while the human allocation rises (PR #343, `Outputs/Tables/H1/Sensitivity/SPD_KK10_hvarpart/spd_kk10__spatial_control_composition.csv`). KK10 and HYDE are smooth fields: within a region their values follow a present-day land-suitability or allocation map, and at a site their trajectory is close to a scaled regional population curve. KK10 population before about 3 ka comes from the climate-driven GLUES model, and HYDE land-use allocation uses BIOME and NPP climate weights. SPD and KK10 agree in level (Kendall tau 0.87) but not in change (first differences 0.07). Human shares of the gridded proxies may therefore partly be spatial structure, temporal trend, or climate.

## Goal
Measure how much of each proxy's human share overlaps with spatial structure (temporal H1), temporal trend (spatial H1), and climate.

## Scope
- [ ] Temporal H1: export the commonality fraction shared by human and space per region × age unit, per proxy; refit with a spatially detrended proxy (residuals of the proxy on the selected dbMEMs).
- [ ] Spatial H1: export the commonality fraction shared by human and time per record, per proxy; refit with a temporally detrended proxy (residuals on linear age).
- [ ] Report each proxy's correlation with the selected climate variables per unit.
- [ ] `fit_varhp()` already partitions with `var.part = TRUE` (`R/functions/hvarpart/fitting/fit_varhp.R`, line 219); extend the exports, which currently keep only `Unique`, `Average.share`, `individual`, and `I.perc(%)`.

## Tests and validation
- [ ] Commonality fractions sum to the total adjusted R² of each model.
- [ ] Detrended proxies are uncorrelated with the detrending terms.

## Acceptance criteria
- [ ] Diagnostics exported for SPD, KK10, and HYDE 3.5 land use at both scales.
- [ ] One supplementary figure summarising human ∩ space, human ∩ time, and detrended balances per proxy.

## Relationships
- Blocked by #[D]
- Part of #317
```

### F. Quantify the overlap between demography and land-use representations of human impact

Labels: H1, 📈⚙️methodology progress, 💻code

```markdown
## Context
H1 is fitted with one human predictor per model, so each proxy's share includes whatever is correlated with it. The single-proxy runs cannot show whether KK10's larger spatial share (55.1 % human-dominant records versus 28.5 % for SPD with the same climate set, PR #343) is information that SPD lacks or the same variation represented differently. Hierarchical partitioning splits shared variance equally between groups and hides this overlap; the commonality fractions show it.

## Goal
Estimate the PAP variation explained uniquely by demography, uniquely by land use, and jointly by both, by fitting them as separate focal groups next to climate.

## Scope
- [ ] Fit three focal groups (climate, demography, land use) with the protected time (spatial H1) or dbMEM (temporal H1) controls and the same shared climate selection as #[D]. Pairings: SPD + KK10 (main); HYDE 3.5 population + KK10 (check; expected near-total overlap because both rest on the same population estimates, within-record r = 0.96 for HYDE 3.2).
- [ ] Fit only where the extra predictor keeps the residual-df minimum; report records dropped in spatial H1.
- [ ] Export commonality fractions: demography only, land use only, demography ∩ land use, and each one's intersection with climate, using the `var.part = TRUE` output of `fit_varhp()`.

## Tests and validation
- [ ] Fractions sum to the total adjusted R² per model.
- [ ] A synthetic test where land use is an exact linear function of demography gives near-zero unique fractions.

## Acceptance criteria
- [ ] Tables exported to `Outputs/Tables/H1/Proxies/Overlap/` (new) for both scales and both pairings.
- [ ] Data for the supplementary overlap figure in #[G].

## Relationships
- Blocked by #[D]
- Part of #317
```

### G. Plot H1 results across human proxies

Labels: H1, 🖼️visualisation, 💻code

```markdown
## Context
H1 is fitted once per human proxy (SPD, KK10 land use, HYDE 3.5 land use; supplementary HYDE 3.5 population and expert events). The figures must show where conclusions agree across proxies and where they depend on the proxy. The current temporal figure pairs SPD and events, each on its own data (`plot_h1_temporal_controlled_composition()`, `R/functions/visualisation/h1_temporal/`); the current spatial figure is a map, density, and climate-zone layout (`plot_h1_spatial_controlled_balance()`, `R/functions/visualisation/h1_spatial/`).

## Goal
Build main and supplementary H1 figures for the multi-proxy design.

## Scope
- [ ] Temporal (main): one column per main proxy, one row per continental region, stacked zero-truncated human, climate, and space allocations through time; each proxy with its full data over its native window, so columns differ in time span and records, as SPD and events do now. Extend `plot_h1_temporal_controlled_composition()` or `plot_human_proxy_temporal_comparison()`.
- [ ] Temporal (supplementary): the same layout on the common cohort, 2,000–8,000 BP; expert events as a separate flagged column.
- [ ] Spatial (main), new function: agreement map, one point per common-cohort record coloured by the number of main proxies with `zero_balance > 0` (0–3).
- [ ] Spatial (main), new function: regional forest plot of the dbMEM-adjusted zero-truncated balance with its interval per continental region and overall, one point per proxy, with small ridgelines of record balances.
- [ ] Spatial (supplementary): the current layout repeated per proxy with `plot_h1_spatial_controlled_balance()` / `plot_human_proxy_spatial_balance_comparison()`.
- [ ] Overlap (supplementary): stacked commonality fractions from #[F] per continental region.
- [ ] Figure size and resolution from the central config, saved with `ggview::save_ggplot()`.

## Tests and validation
- [ ] testthat tests for the new plotting functions (returns a ggplot, required columns checked).
- [ ] Every plotted value reconstructed from the exported tables of #[D] and #[F].

## Acceptance criteria
- [ ] PNG and PDF exported to `Outputs/Figures/H1/Proxies/` (new) and inspected.

## Relationships
- Blocked by #[D]
- Blocked by #[F]
- Part of #317
```

### H. Report single-proxy H1 evidence to the co-authors and record the design decision

Labels: 🗣️communication, 📝method documentation

```markdown
## Context
The previous co-author memo (`Manuscript/COMMSENV-25-2408/R1/Internal/human_proxy_hvarpart_decision_memo.qmd`) compared joint human blocks. The design is now one human predictor per model, with hypotheses and a headline rule fixed in #[B]: a conclusion is stated generally only if it holds for every main proxy (SPD, KK10 land use, HYDE 3.5 land use) in the main results and is not overturned on the common cohort (units with all three proxies, 2,000–8,000 BP); otherwise it is stated per construct.

## Goal
Present the single-proxy results to the co-authors and record their decision on design, proxy set, windows, and framing.

## Scope
- [ ] Write a successor memo in `Manuscript/COMMSENV-25-2408/R1/Internal/` with inline R only: results per proxy and scale, the headline-rule outcome, diagnostics (#[E]), overlap (#[F]), figures (#[G]).
- [ ] Add an exploratory H2 preview using region × climate-zone proxy means from #[C] in `prepare_h2_hvarpart_data()`, labelled as mixing smoothed (brms) and unsmoothed predictors.
- [ ] Record the decision in this issue and tick the second decision point in #317.

## Tests and validation
- [ ] The memo renders; no number is hand-typed.

## Acceptance criteria
- [ ] Decision recorded: design, proxy set, windows, framing (scale divergence; necessary but not sufficient; representation dependence), roles of HYDE population and events.

## Relationships
- Blocked by #[D]
- Blocked by #[E]
- Blocked by #[F]
- Blocked by #[G]
- Part of #317
```

### I. Refit H2 once per human proxy

Labels: H2, 📈⚙️methodology progress, 💻code

```markdown
## Context
H2 asks whether human activity is the predominant predictor of changes in the relationships among PAPs. The response is a Procrustes distance between PCA ordinations of PAPs for successive ages per continental region × climate zone (`data_m2_filtered`, store `data_preparation/paps`). The predictors are brms trend predictions per region × climate zone from the temporal models (`data_temporal_model_predictions`, store `temporal_models/exports`), built by `prepare_predictor_model_data()` (`R/functions/modelling/preparation/prepare_predictor_model_data.R`) for the analysis `predictor_temporal`, currently SPD plus the four climate variables. `R/analyses/04_h2_multidimensional_shifts/pipeline.R` fits `output_hvar_h2_spd` with `fit_hvarpart_models()` and hard-coded `predictor_vars`; `R/analyses/05_visualisations/h2/predictor_interrelationships.R` reads `output_hvar_h2_spd`, which is not a public target. H1 now runs once per human proxy, so H2 must use the same proxies.

New brms fits follow the protected temporal-model lifecycle: they run only for current rows in `Data/Temporal_models/general_model_run_requests.csv` (`select_authorized_temporal_model_runs()`), and a request is used up once `fit_started` is written.

## Goal
Refit H2 once per main human proxy (SPD, KK10 land use, HYDE 3.5 land use), with one human predictor per model.

## Scope
- [ ] Add KK10 and HYDE 3.5 land-use fractions (regional values from #[C]) to `prepare_predictor_model_data()` and the `predictor_temporal` set, with a zero-one-inflated beta family for land-use fractions.
- [ ] Prepare fit requests for the new predictor models and ask for authorisation in `general_model_run_requests.csv`; do not fit without it.
- [ ] Generalise `prepare_h2_hvarpart_data()` (`R/functions/data/preparation/prepare_h2_hvarpart_data.R`) and the hard-coded `predictor_vars` so that the human predictor comes from a profile; apply the same response-free climate selection rule as H1.
- [ ] Make `output_hvar_h2_<proxy>` public targets in `pipeline_contracts.csv`.
- [ ] Export H2 allocations per proxy on the same groups to `Outputs/Tables/H2/Proxies/` (new).

## Tests and validation
- [ ] Tests for `prepare_h2_hvarpart_data()` with each proxy.
- [ ] Model diagnostics exported for every new brms fit.

## Acceptance criteria
- [ ] Fits ran only from authorised ledger rows.
- [ ] H2 results for every main proxy on identical groups.

## Relationships
- Blocked by #[C]
- Blocked by #[H]
- Part of #317
```

### J. Rerun the R1 robustness checks for each human proxy

Labels: H1, 📈⚙️methodology progress, 💻code

```markdown
## Context
All R1 robustness checks were run with SPD and all four climate predictors (closed issues #325, #326, #327, #329, #333). H1 now runs once per human proxy (SPD, KK10 land use, HYDE 3.5 land use) with a climate set selected once per unit without the response (`collinear`, `max_cor = 0.8`, `max_vif = 5`), so each check must be repeated per proxy.

## Goal
Show which R1 robustness results hold for each main human proxy.

## Scope
- [ ] Neighbourhood size: SPD strict 250 versus 500 km (as in `R/analyses/91_sensitivity_analyses/spd_radius/pipeline.R`); extraction radius for KK10 and HYDE 3.5 (cell, 10, 25, 50 km).
- [ ] Spatial thinning (250 and 500 km, 100 repetitions) and leave-one-continental-region and leave-one-climate-zone-out (as in `R/analyses/91_sensitivity_analyses/spatiotemporal_robustness/pipeline.R`).
- [ ] Residual Moran's I before and after spatial control.
- [ ] Reduced PAP response set (as in `R/analyses/91_sensitivity_analyses/predictor_collinearity/pipeline.R`).
- [ ] Signed, zero-truncated, and exclude-negative HVarPart profiles.
- [ ] Total and partial adjusted R² per proxy.
- [ ] Expert events as a supplementary, flagged sensitivity (as in `R/analyses/91_sensitivity_analyses/human_event_inclusion/pipeline.R`), including records whose expert assessment found no events (zeros are confirmed absences); plus a variant with PAPs recomputed without the event indicator taxa (*Plantago*, cereals, Poaceae, *Calluna*).

## Tests and validation
- [ ] Each check runs in its own store and does not overwrite baseline stores.

## Acceptance criteria
- [ ] A comparison table per check and proxy: share of human-dominant records, overall adjusted balance, human-dominant temporal units, and headline-rule outcome.

## Relationships
- Blocked by #[D]
- Blocked by #[H]
- Part of #317
```

### K. Test the single-proxy design against reviewer-style objections

Labels: H1, 📈⚙️methodology progress, 💻code

```markdown
## Context
An internal pre-review of the one-predictor-per-proxy design found these objections, each testable:
1. Reducing climate to fewer variables lowers its capacity and can shift every human–climate balance towards humans; the balance depends on how many variables represent each group. The KK10 and HYDE runs that moved towards humans would also gain from any loss of climate capacity.
2. SPD is summed over 250–500 km, while KK10 and HYDE are extracted at the pollen-source scale, so proxy differences mix construct and spatial scale.
3. HYDE has millennial steps before 1 CE; interpolation to 500-year ages makes it smoother than its evidence and can inflate within-record fits (about 13 ages per record over 8,000–2,000 BP).
4. Linear distance-based RDA may rank proxies by how linear their relationship is, not by importance.
5. SPD calibration and sampling uncertainty is not propagated.
6. The main temporal results use each proxy's full data and native window, so proxy differences may reflect which records and periods enter.

## Goal
Measure how much each objection changes the results and the headline-rule outcome.

## Scope
- [ ] Climate representation: refit with all four climate variables where residual df allow, the shared filtered selection (main), and climate principal components (first components explaining at least 90 % of climate variance per unit, proposed).
- [ ] Scale-matched: KK10 and HYDE 3.5 averaged over each record's SPD buffer (extraction from #[C]).
- [ ] Native steps: only ages where the proxy has an exact layer (`is_native_layer` from #[C]).
- [ ] Transformations: rank and log(x + small constant) versions of every proxy.
- [ ] If feasible: SPD recomputed from bootstrapped date sets (proposed 100 replicates) for spatial H1.
- [ ] Common cohort: every main comparison repeated on units with all main proxies, 2,000–8,000 BP.

## Tests and validation
- [ ] Each sensitivity has its own store and does not overwrite main runs.

## Acceptance criteria
- [ ] One table: per sensitivity and proxy, the change in human-dominant record share, overall adjusted balance, human-dominant temporal units, and whether the headline-rule outcome changes.

## Relationships
- Blocked by #[D]
- Blocked by #[H]
- Part of #317
```

### Optional. Compare single-PAP and whole-assemblage attribution across human proxies

Create only if the co-authors put it in scope. Labels: H1, Not decided

```markdown
## Context
Studies attributing Holocene vegetation change mainly to people mostly use one property, often diversity. Our H1 response is the multivariate set of ten PAPs (`response_vars_h1`). If single properties lean towards humans while the whole assemblage does not, that reconciles our result with that literature.

## Goal
Compare human–climate balances for individual PAPs with the multivariate response, per human proxy.

## Scope
- [ ] Add single-PAP response specifications (at least `n0`, `n1`, `n2`, `dcca_axis_1`, `roc`) to the H1 input pipeline.
- [ ] Run spatial and temporal H1 per proxy with the shared climate selection from #[D].
- [ ] Export balances per PAP, region, and proxy.

## Acceptance criteria
- [ ] Comparison table and one figure.

## Relationships
- Blocked by #[D]
- Part of #317
```
