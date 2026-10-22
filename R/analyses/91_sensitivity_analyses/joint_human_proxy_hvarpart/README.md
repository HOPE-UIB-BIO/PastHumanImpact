# Human-proxy H1 HVarPart profiles

The shared `pipeline.R` fits within-dataset HVarPart with time control, spatially aggregates the resulting human–climate balance, and fits HVarPart within continental-region age slices with spatial dbMEM control.
Analysis variants are registered in `R/analyses/00_profiles/analysis_profiles.csv` and use separate target stores.

| Runner | Human predictors | External store below `Targets_data/` | Outputs |
| --- | --- | --- | --- |
| `00_run.R` | √SPD, KK10, √HYDE | `sensitivity_analyses/joint_human_proxy_hvarpart` | Existing joint-proxy output directories |
| `run_kk10.R` | KK10 fraction only | `sensitivity_analyses/kk10_only_hvarpart` | `Outputs/Figures/H1/Sensitivity/KK10_only_hvarpart/` and `Outputs/Tables/H1/Sensitivity/KK10_only_hvarpart/` |
| `run_hyde.R` | √HYDE population only | `sensitivity_analyses/hyde_only_hvarpart` | `Outputs/Figures/H1/Sensitivity/HYDE_only_hvarpart/` and `Outputs/Tables/H1/Sensitivity/HYDE_only_hvarpart/` |
| `run_spd_kk10.R` | √SPD and KK10 fraction | `sensitivity_analyses/spd_kk10_hvarpart` | `Outputs/Figures/H1/Sensitivity/SPD_KK10_hvarpart/` and `Outputs/Tables/H1/Sensitivity/SPD_KK10_hvarpart/` |

Run from the repository root:

```r
source("R/analyses/91_sensitivity_analyses/joint_human_proxy_hvarpart/run_kk10.R")
```

Each single-proxy runner explicitly passes its `human_proxy_hvarpart_profile` to the shared graph in the same R process.
It uses the global environment where project configuration sources the functions, allowing targets to fingerprint changes to shared helpers, and removes the profile input after execution.
The default graph and original runner select the joint proxy block.

## KK10-only design

- Retain the existing 8,000–2,000 years BP window and 500-year slices, which match the available cached KK10 product.
- Join the public `data_human_proxy_kk10_primary` product directly; do not require complete SPD or HYDE values from the joint convergence table.
- Preserve KK10 land-cover fractions without a transformation, then retain the existing group scaling during fitting.
- Keep the four climate predictors (`temp_annual`, `temp_cold`, `prec_summer`, `prec_win`), response variables, seeds, permutations, structural controls, and eligibility checks used by the joint sensitivity.
- Retain the established SPD-derived 250 km support with a 500 km fallback for spatial comparability; SPD values are excluded from the human predictor block.
- Reuse canonical H1 dataset-age inputs and metadata; this run inherits their upstream eligibility and is not a reconstruction of the pollen input population.
- Use the general H1 time-slice input rather than the explicitly SPD-filtered slice table.

## HYDE-only design

Run `source("R/analyses/91_sensitivity_analyses/joint_human_proxy_hvarpart/run_hyde.R")` from the repository root.
Use the same age interval, climate predictors, structural controls, thresholds, buffer support, and inherited H1 input eligibility as the KK10 profile above.
Join `data_human_proxy_hyde_primary` directly, without matching SPD or KK10 values.
Preserve zero population and apply the existing square-root transformation to HYDE population sums before group scaling.
Write the HYDE figures and `hyde__*.csv` tables to their own directories, including `hyde__provenance.csv` and HYDE-labelled result records.

## SPD plus KK10 design

Run `source("R/analyses/91_sensitivity_analyses/joint_human_proxy_hvarpart/run_spd_kk10.R")` from the repository root.
The human group contains √SPD and the untransformed KK10 fraction; HYDE is excluded from the predictors.
Use the same age interval, climate predictors, structural controls (time in the within-dataset models, spatial dbMEM in the spatial aggregation and the time-slice models), thresholds, and buffer support as the joint profile.
Use the SPD-filtered H1 time-slice input, as the joint profile does, because SPD enters the human group.
Take √SPD and KK10 from the fully matched `data_human_proxy_matches` product. Its coverage equals the KK10-only and HYDE-only products, so requiring a HYDE match removes no observations.
Write the figures and `spd_kk10__*.csv` tables to their own directories.

## Execution and review

Inspect the KK10 manifest, check selected predictors and input coverage, then build the isolated store.
Review fit-status tables as well as target completion, because ineligible designs and model errors are retained as explicit statuses.
The exported `kk10__provenance.csv` records the selected human and climate columns, age interval, upstream fingerprints, support, and modelling thresholds.
The result-record table carries the KK10 profile identifiers and configuration hashes.
