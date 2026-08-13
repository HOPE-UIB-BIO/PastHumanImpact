# Analysis structure migration

This manifest records the coordinated migration from publication-role and manuscript-number paths to stable analytical paths. It is intended to remain useful when manuscript figure numbers change.

## Function directories

| Previous path | Canonical path | Ownership rule |
| --- | --- | --- |
| `R/functions/data_wrangling/` | `R/functions/data/factors/`, `data/storage/`, and `data/preparation/` | Factor labelling, persistent storage, and data preparation are separated. |
| `R/functions/supplementary_analyses/` | `R/functions/data/pollen/` | Pollen-count diagnostics are data functions, not publication-role functions. |
| `R/functions/events/` | `R/functions/human_impact/events/` | Event proxies belong to the human-impact domain. |
| `R/functions/PAPs/` | `R/functions/human_impact/paps/` | PAP functions use a lower-case, domain-owned path. |
| `R/functions/spd/` | `R/functions/human_impact/spd/` | SPD functions belong to the human-impact domain. |
| `R/functions/configuration/` | `R/functions/workflow/configuration/` | Repository configuration and provenance belong to workflow infrastructure. |
| `R/functions/testing/` | `R/functions/workflow/testing/` | Test runners and contract audits belong to workflow infrastructure. |
| `R/functions/hvarpart/` | `R/functions/hvarpart/components/`, `diagnostics/`, `fitting/`, and `preparation/` | HVarPart functions are separated by computational responsibility. |
| `R/functions/modelling/` | `R/functions/modelling/configuration/`, `fitting/`, `prediction/`, `preparation/`, and `provenance/` | Temporal-model functions are separated by lifecycle responsibility. |
| `R/functions/spatial/` | `R/functions/spatial/dbmem/`, `diagnostics/`, `hvarpart/`, and `thinning/` | Generic spatial mechanics remain separate from spatial HVarPart analysis. |
| Temporal helpers formerly under `R/functions/hvarpart/` | `R/functions/temporal/` | Ordered permutations, temporal distance, age standardisation, and temporal Moran diagnostics are model-independent temporal infrastructure. |
| `R/functions/visualisation/` | `R/functions/visualisation/common/`, `data/`, `diagnostics/`, `h1_spatial/`, `h1_temporal/`, and `h2/` | Plot functions are grouped by analytical product rather than publication role. |

Function basenames follow the verb contract in `.ai/r-functions.md` and exactly match their function names. `R/00_Config_file.R` sources the function tree recursively, so directory migrations do not change function discovery.

## Visualisation scripts

| Previous script | Canonical script |
| --- | --- |
| `R/visualisations/01_Figure_2_H1_spatial.R` | `R/visualisations/h1/spatial/human_climate_balance.R` |
| `R/visualisations/02_Figure_3_H1_temporal.R` | `R/visualisations/h1/temporal/human_climate_space_composition.R` |
| `R/visualisations/03_Figure_4_H2_interrelationships.R` | `R/visualisations/h2/predictor_interrelationships.R` |
| `R/visualisations/extended_data_analysis/EDA_1_pollen_data.R` | `R/visualisations/data/pollen_dataset_coverage.R` |
| `R/visualisations/extended_data_analysis/EDA_2_human_presence.R` | `R/visualisations/data/human_impact_coverage.R` |
| `R/visualisations/extended_data_analysis/EDA_3_predictor_trends.R` | `R/visualisations/h1/temporal/predictor_trends.R` |
| `R/visualisations/extended_data_analysis/EDA_4_example_records.R` | `R/visualisations/h1/dataset_trends/example_datasets.R` |
| `R/visualisations/extended_data_analysis/EDA_5_pap_temporal_trends.R` | `R/visualisations/h1/temporal/pap_trends.R` |
| `R/visualisations/extended_data_analysis/EDA_6_core_temporal_trends.R` | `R/visualisations/h1/dataset_trends/all_dataset_trends.R` |
| `R/visualisations/extended_data_analysis/EDA_7_event_temporal_trends.R` | `R/visualisations/h1/temporal/event_trends.R` |
| `R/visualisations/extended_data_analysis/Map_of_all_records.R` | `R/visualisations/data/dataset_locations.R` |

## Generated figure directories

| Previous directory | Canonical directory |
| --- | --- |
| Top-level numbered generated figures | `Outputs/Figures/Overview/`, `H1/Spatial/`, `H1/Temporal/`, or `H2/Interrelationships/` according to analysis |
| `Outputs/Figures/Extended_data_figures/Collinearity/` | `Outputs/Figures/H1/Spatial/` |
| `Outputs/Figures/Extended_data_figures/HVarPart/` | `Outputs/Figures/H1/Spatial/`, `H1/Temporal/`, `H2/Interrelationships/`, or `Diagnostics/HVarPart/` according to content |
| `Outputs/Figures/Extended_data_figures/Map/` | `Outputs/Figures/Data/` |
| `Outputs/Figures/Extended_data_figures/Spatial_dependence/` | `Outputs/Figures/H1/Spatial/` and `H1/Temporal/` according to analysis |
| Other files under `Outputs/Figures/Extended_data_figures/` | `Outputs/Figures/Data/` or `H1/Temporal/` according to content |
| `Outputs/Figures/Supplementary_analyses/` | `Outputs/Figures/Data/` |
| `Outputs/Figures/Supplementary_figures/Core_temporal_trends/` | `Outputs/Figures/H1/Dataset_trends/` with each `dataset_*` basename preserved |
| `Outputs/Figures/Supplementary_figures/Core_examples/` | `Outputs/Figures/H1/Dataset_trends/` with `core` replaced by `dataset` |
| `Outputs/Figures/Supplementary_figures/Event_temporal_trends/` | `Outputs/Figures/H1/Temporal/Event_trends/` |
| `Outputs/Figures/Supplementary_figures/PAP_temporal_trends/` | `Outputs/Figures/H1/Temporal/PAP_trends/` with the `pap_trend_*` naming pattern |
| `Outputs/Figures/Supplementary_figures/Predictor_temporal_trends/` | `Outputs/Figures/H1/Temporal/Predictor_trends/` with the `predictor_trend_*` naming pattern |
| `Outputs/Figures/Model_diagnostics/` | `Outputs/Figures/Diagnostics/Temporal_models/` |
| Manually edited comparison figures | `Manuscript/_internal/figure_references/` |

## Main analytical products

| Previous stem | Canonical stem |
| --- | --- |
| `Figure2_h1_spatial` | `H1/Spatial/human_climate_balance_time_and_space_controlled` |
| `Figure2_h1_spatial_baseline` | `H1/Spatial/human_climate_balance_human_climate_only` |
| `Figure2_h1_spatial_signed_full_range` | `H1/Spatial/human_climate_balance_untruncated_hierarchical_contributions` |
| `Figure2_h1_spatiotemporal_allocation` | `H1/Spatial/human_climate_time_zero_truncated_hierarchical_composition` |
| `Figure2_h1_spatiotemporal_signed` | `H1/Spatial/human_climate_time_untruncated_hierarchical_contributions` |
| `Figure2_h1_spatiotemporal_pure` | `H1/Spatial/human_climate_time_unique_adjusted_r2` |
| `Figure3_h1_temporal` | `H1/Temporal/HVarPart/human_climate_space_zero_truncated_hierarchical_composition` |
| `Figure3_h1_temporal_baseline` | `H1/Temporal/HVarPart/human_climate_composition_human_climate_only` |
| `Figure3_h1_temporal_signed_full_range` | `H1/Temporal/HVarPart/human_climate_only_untruncated_hierarchical_contributions` |
| `Figure3_h1_spatiotemporal_signed` | `H1/Temporal/HVarPart/human_climate_space_untruncated_hierarchical_contributions` |
| `Figure3_h1_spatiotemporal_pure` | `H1/Temporal/HVarPart/human_climate_space_unique_adjusted_r2` |
| `Figure4_h2` | `H2/Interrelationships/predictor_interrelationships` |
| `Figure4_h2_signed_full_range` | `H2/Interrelationships/predictor_interrelationships_untruncated_hierarchical_contributions` |

The same basename rule applies to PNG and PDF products. Reviewer sensitivity products use `spatiotemporal_control_*`, `spatial_control_*`, `human_climate_balance_*`, or `time_control_*` stems that describe the calculation directly.

## Symbol and profile migration

| Previous name | Canonical name |
| --- | --- |
| `analyse_temporal_hvarpart_core()` | `fit_temporal_hvarpart_dataset()` |
| Previous batch `analyse_temporal_hvarpart_dataset()` | `fit_temporal_hvarpart_datasets()` |
| `collapse_hvar_core_age_data()` | `aggregate_hvar_dataset_ages()` |
| `calculate_three_group_partial_fractions()` | `compute_three_group_unique_adjusted_r2()` |
| `prepare_original_figure2_records()` | `prepare_human_climate_only_records()` |
| `prepare_spatial_hvarpart_stack()` | `prepare_spatial_hvarpart_composition()` |
| `plot_reciprocal_figure2_balance()` | `plot_h1_spatial_controlled_balance()` |
| `plot_reciprocal_figure2_components()` | `plot_h1_spatial_control_profiles()` |
| `plot_reciprocal_figure3_stack()` | `plot_h1_temporal_controlled_composition()` |
| `plot_reciprocal_figure3_diagnostics()` | `plot_h1_temporal_control_profiles()` |
| `baseline` model profile | `human_climate_only` |
| `baseline` spatial-sensitivity reference | `unthinned` |
| Pre-filter diagnostic stage `baseline` | `unfiltered` |
| `signed` exported profile | `untruncated_hierarchical_contributions` |
| `allocation` exported profile | `zero_truncated_hierarchical_composition` |
| `pure_fractions` or `partial_fractions` exported profile | `unique_adjusted_r2` |

Internal mathematical columns may retain established short names where changing the schema would obscure a standard calculation, but exported tables, figures, target names, result keys, and report labels use the canonical vocabulary.
