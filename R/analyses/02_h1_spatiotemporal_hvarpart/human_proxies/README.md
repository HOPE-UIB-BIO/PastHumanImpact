# Single-proxy H1 runs

These pipelines fit spatial and temporal H1 once per human proxy, with exactly one human predictor block per model and a climate block selected once per analytical unit and shared by every proxy in that unit. They implement decisions D1, D2, D5, D6, D8, D9, D10, D11, D12, and D14 of `Data/Plans/decision_record_human_impact_reframing_2026-11-16.md`. The reference H1 pipelines in the other folders of `02_h1_spatiotemporal_hvarpart/` are unchanged.

## Runs

Each run is one row of `R/analyses/00_profiles/analysis_profiles.csv` with `configuration_reference == "human_proxy_h1"`. A run is defined by its human proxy, its spatial support (`proxy_support`), its window (`age_window`), and its scale (`analytical_unit`):

| Proxy | Support | Role |
| --- | --- | --- |
| `spd` (square root) | `spd_buffer` | main |
| `kk10` (land-use fraction) | `spd_buffer` | main |
| `hyde_3_5_land_use` (land-use fraction) | `spd_buffer` | main |
| `kk10`, `hyde_3_5_land_use` | `radius_50km` | sensitivity |
| `hyde_3_5_population` (square root) | `spd_buffer` | supplementary, core run only |
| `events` (the nine event groups) | `not_applicable` | supplementary, core run only |

- Windows: `common_cohort` is 2,000–8,000 BP on the common cohort; `native` is the proxy's native window on all records with that proxy (SPD 2,000–11,500 BP, KK10 500–8,000 BP, HYDE 3.5 500–11,500 BP, events 500–11,500 BP). Native windows are read from the human-proxy data layer.
- Scales: `within_dataset` is spatial H1 (one time-controlled HVarPart model per record, aggregated with dbMEM spatial control); `time_slice` is temporal H1 (one spatially controlled HVarPart model per continental region and 500-year age).
- Every run has its own store, `analyses_h1/<profile_id>`, and its own output folder, `Outputs/Tables/H1/Proxies/<profile_id>/`.

## Inputs and shared climate selection

`01_inputs/pipeline.R` (store `analyses_h1/human_proxy_inputs`) builds:

- Record histories per window from the uncut PAPs (`data_properties`) and predictors (`data_predictors`, 0–12,000 BP), filtered (private Latin American records and Africa removed) and age-collapsed as in the reference H1 inputs, with one column per proxy and support from `data_human_proxy_values` (`value_transformed`).
- The common cohort: records with a value of SPD, KK10, and HYDE 3.5 land use at the SPD buffer and of KK10 and HYDE 3.5 land use at 50 km at every age of 2,000–8,000 BP, so main and 50 km runs share identical records. Records without SPD at either radius (SPD stored as zero, a fill value) are set to missing SPD and are therefore excluded from SPD runs and from the cohort.
- Region-by-age units for the common cohort and for all records.
- The climate selection, made once per record and window and once per region-by-age unit from the climate columns only (`temp_annual`, `temp_cold`, `prec_summer`, `prec_win` in that preference order; `collinear` filter with `max_cor = 0.8`, `max_vif = 5`). Every run reuses it unchanged; only the human block is filtered per run. In region-by-age units, records without the proxy at that age are removed before fitting.

## Decision metrics

- Spatial H1: the overall dbMEM-adjusted human-minus-climate balance of the estimable records. Its 95 % interval is a percentile bootstrap (1,999 replicates) that resamples records within region-by-climate-zone strata and refits the weighted aggregation model with the dbMEMs selected in the observed fit.
- Temporal H1: the pooled human-minus-climate allocation over estimable region-by-age units, a ratio of sums as in `summarise_hvarpart_importance()`, with the number of units where human exceeds climate. Its 95 % interval is a moving-block bootstrap (blocks of three consecutive ages within each region, regions as strata, 1,999 replicates).
- Profiles: zero-truncated (headline), signed, and exclude-negative (only records or units without a negative contribution).
- Classes: human (interval above zero), climate (below zero), no clear dominance (includes zero).

## Outcomes

`04_outcomes/pipeline.R` (store `analyses_h1/human_proxy_outcomes`, tables in `Outputs/Tables/H1/Proxies/Outcomes/`):

- stops with an error unless every run used the shared climate selection of each unit and all common-cohort runs of a scale had identical units;
- recomputes the decision metrics of SPD, KK10, and HYDE 3.5 land use on the units estimable for all three (shared records), for both windows and for both supports of the gridded proxies (SPD stays at its buffer), and takes each run's own metric on all its estimable units (all records);
- writes the outcome table as a full factorial: for each scale and proxy, one class per support of the gridded proxies (`spd_buffer`, `radius_50km`), window (`common_cohort`, `native`), and record set (`shared_records`, `all_records`), with the per-profile classes and a flag when the class changes between profiles. The supplementary proxies have all-records cells only. The decision-record cells are `spd_buffer__common_cohort__shared_records` (main window) and `spd_buffer__native__all_records` (extension);
- checks the code: the reference matched-SPD model of the collinearity-managed sensitivity is refitted with this code from its own inputs and must reproduce every reference record within 1e-8, or the pipeline stops;
- reports, without a threshold, how the SPD common-cohort run on the current PAPs differs from that reference. The reference used the PAPs held in the reference H1 inputs store (`analyses_h1/inputs`, built 2026-08-14), an older version than the PAP store used here.

## Figures

Three scripts in `R/analyses/05_visualisations/h1/human_proxies/` read the stores and write to `Outputs/Figures/H1/Proxies/`:

- `outcome_table.R` (`Outcomes/`): the zero-truncated balance and 95 % interval of every main-proxy cell of the outcome table, coloured by outcome class (`plot_human_proxy_outcome_table()`).
- `spatial_balance.R` (`Spatial/`): the reference spatial-balance layout (`plot_h1_spatial_controlled_balance()`) for each main proxy in the two decision-record cells: common cohort with shared records, and native window with all records. A few records in a Koppen class outside the project's eleven regions cannot be mapped; the title counts them.
- `temporal_composition.R` (`Temporal/`): the human, climate, and space composition of every region-by-age unit (`plot_h1_temporal_joint_human_proxy_composition()`) for the same cells.

## Run

```r
source("R/00_Config_file.R")
source("R/analyses/02_h1_spatiotemporal_hvarpart/human_proxies/00_run.R")
```

The runner builds the inputs, the 28 run stores in turn, and the outcomes. The run scripts resolve their profile from the store passed by the runner (the `script_profile` column of `pipeline_contracts.csv`). For a trial run, a copy of a run script with `region_subset` set to one continental region and a separate store and table folder restricts the run to that region.
