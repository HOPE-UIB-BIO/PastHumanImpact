# Revised manuscript and reviewer response

This directory is an independent Quarto project for the revised article and
the point-by-point response.

The submitted-version scaffold is preserved in `../Original/`. The revised
article and its response materials are contained in this `R1/` directory, with
the historical response modules nested under `Reply/`.

Run commands from the repository root.

## Refresh reporting evidence

The PAP export is reconstructed from the existing external prediction store;
this does not fit temporal models.

```powershell
Rscript -e "library(here); source(here::here('R/00_Config_file.R')); script <- here::here('R/analyses/03_temporal_models/06_exports/pipeline.R'); store <- resolve_pipeline_store_path(data_storage_path = data_storage_path, store_relative_path = 'temporal_models/exports'); targets::tar_make(names = c('table_pap_temporal_predictions', 'file_pap_temporal_predictions'), script = script, store = store)"
Rscript R/analyses/06_reporting/00_run.R
```

## Assemble publication assets

```powershell
Rscript Manuscript/COMMSENV-25-2408/R1/scripts/assemble_revision_assets.R
```

The assembler copies only registered figure sources into `figures/`, rejects
missing or duplicate mappings, and writes
`evidence/revision-artifact-manifest.csv` with source and destination hashes.

## Render the revision package

```powershell
quarto render Manuscript/COMMSENV-25-2408/R1/manuscript.qmd
quarto render Manuscript/COMMSENV-25-2408/R1/Reply/response-to-reviewers.qmd
```

Expected outputs are written to `rendered/`:

- `Felde_Mottl_Flantua_et_al_HumanImpact_revised.pdf`
- `Felde_Mottl_Flantua_et_al_HumanImpact_revised.docx`
- `Reply/response-to-reviewers.pdf`
- `Reply/response-to-reviewers.docx`

The manuscript presents the Main Text and Figs. 1–4, followed by the
full Methods section. A separately labelled Supplementary Information section
contains supplementary results, Supplementary Figs. S1–S40, Supplementary
Tables S1–S10, and the relevant references. The journal calls this material “Supplementary Information”; the
revision therefore does not use “Extended Data” terminology.

Author decisions on expert provenance, reviewer-suggested references, and the two scope-boundary responses are recorded in the response modules. No `AUTHOR REVIEW TODO` callouts remain in the current revision package.
