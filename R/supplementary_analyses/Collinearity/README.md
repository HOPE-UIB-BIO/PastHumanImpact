# PAP Collinearity Analysis

This folder contains runner, pipeline entry, and visualisation scripts for reviewer collinearity analyses.

## Entry Scripts

- `01_run_reviewer_collinearity.R`: canonical runner for the full reviewer collinearity workflow.
- `05_pipeline_h1_reviewer_collinearity.R`: local pipeline entry script that forwards to the canonical targets definition in `R/target_pipelines/`.
- `02_run_collinearity_outputs.R`: orchestrator for visualisation outputs.

## Visualisation Scripts

- `pap_collinearity_correlation.R`: correlation structure among PAP variables.
- `pap_collinearity_hvarpart_influence.R`: baseline vs reduced predictor importance ratio differences.
- `human_climate_balance_reduced_predictors.R`: simplified spatial balance chart using reduced predictors.

## Output Locations

- Tables: `Outputs/Tables/Collinearity/`
- Figures: `Outputs/Figures/H1/Spatial/`

## Related Readmes

- `Outputs/Tables/Collinearity/README.md`
- `Outputs/Figures/H1/Spatial/README.md`
