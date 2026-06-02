# Collinearity Supplementary Bundle

This folder contains runner, pipeline entry, and visualisation scripts for reviewer collinearity analyses.

## Entry Scripts

- `01_run_reviewer_collinearity.R`: canonical runner for the full reviewer collinearity workflow.
- `05_pipeline_h1_reviewer_collinearity.R`: local pipeline entry script that forwards to the canonical targets definition in `R/target_pipelines/`.
- `02_run_collinearity_outputs.R`: orchestrator for visualisation outputs.

## Visualisation Scripts

- `EDA_5a_pap_collinearity_correlation.R`: correlation structure among PAP variables.
- `EDA_5b_pap_collinearity_hvar_difference.R`: baseline vs reduced predictor importance ratio differences.
- `EDA_5c_h1_spatial_reduced_simple.R`: simple Figure 2 style chart using reduced predictors.

## Output Locations

- Tables: `Outputs/Tables/Collinearity/`
- Figures: `Outputs/Figures/Extended_data_figures/Collinearity/`

## Related Readmes

- `Outputs/Tables/Collinearity/README.md`
- `Outputs/Figures/Extended_data_figures/Collinearity/README.md`
