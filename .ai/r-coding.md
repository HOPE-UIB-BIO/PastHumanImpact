# R Coding Guidance

Canonical R guidance for scripts, target pipelines, data processing, modelling,
and visualisation in PastHumanImpact.

## Scope

Apply this file to all `.R` files. For functions and tests, also read
`.ai/r-functions.md`.

Main project areas:

- `R/main_analysis/` - ordered analysis scripts used by `R/01_run_project.R`
- `R/target_pipelines/` - `{targets}` pipeline definitions
- `R/functions/` - reusable project functions
- `R/predictor_models/` - model preparation, fitting, checks, and prediction
- `R/spd_calculation/` - radiocarbon SPD calculation scripts
- `R/visualisations/` - manuscript and exploratory figures

## Project Setup

Use `R/00_Config_file.R` as the shared project setup. It restores `renv`, loads
packages, sources `R/functions/`, defines core constants, and resolves
`data_storage_path`.

At the top of executable scripts:

```r
library(here)

source(
  here::here("R/00_Config_file.R")
)
```

Use `R/___Init_project___.R` only for one-time machine setup.

## Script Structure

- One script should have one clear purpose.
- Keep the existing project banner style for top-level scripts.
- Use section headers with a `-----` suffix so IDE navigation works.
- Put setup, inputs, transformations, outputs, and checks in separate sections.
- Keep scripts runnable from a clean R session.

Section header pattern:

```r
#----------------------------------------------------------#
# 1. Prepare input data -----
#----------------------------------------------------------#
```

## Naming

Use `snake_case` for objects, arguments, and functions.

Prefer type prefixes for important objects:

- `data_*` for data frames and tibbles
- `table_*` for summary tables
- `list_*` for lists
- `vec_*` for vectors
- `mat_*` for matrices
- `mod_*` for models
- `plot_*` for plots
- `path_*` for paths
- `flag_*` for logical flags
- `res_*` for function return objects

Use descriptive full words. Avoid unclear abbreviations.

Function names should be verbs. Data objects should be nouns.

## Formatting

- Use `<-` for assignment.
- Use 2-space indentation.
- Keep R code and roxygen lines to about 80 characters.
- Use `TRUE` and `FALSE`, not `T` and `F`.
- Prefer one argument per line for multi-argument calls.
- Use explicit argument names where practical.

When the right-hand side is a function call, put it on the next line:

```r
data_records <-
  readr::read_csv(path_records)
```

Scalar literals may stay on one line:

```r
min_age <- 0
flag_rerun <- FALSE
```

Control-flow conditions are multi-line:

```r
if (
  base::nrow(data_records) == 0L
) {
  cli::cli_abort("No records available.")
}
```

## Namespaces and Packages

- Use `pkg::function()` for non-base calls.
- Do not add new package dependencies casually.
- Do not call `library()` inside functions.
- Keep package loading centralized in `R/00_Config_file.R` unless a script is a
  minimal reproducible debug script.

## Paths and Data

- Use `here::here()` for repository paths.
- Use `file.path(data_storage_path, ...)` for externally stored project data.
- Do not hardcode machine-specific absolute paths.
- Respect the data layout described in `README.md`.
- Do not silently overwrite expensive outputs unless a clear `rewrite`,
  `rerun`, or `flag_*` control exists.

## Tidyverse

Prefer tidyverse tools when they make data manipulation clearer:

- `dplyr::mutate()`, `dplyr::filter()`, `dplyr::select()`
- `dplyr::join_by()` for joins
- `purrr::map*()` for structured iteration
- `stringr::str_glue()` and `stringr::str_c()` for strings
- `readr::read_csv()` / `readr::write_csv()` for CSV files

Avoid:

- `paste()` and `paste0()` for new string construction
- `apply()`, `lapply()`, `sapply()`, `vapply()`, `mapply()` in new code
- `$` for data-frame column extraction in new code; prefer
  `dplyr::pull()` or `.data[[column_name]]`
- `eval(parse(...))`

Always `dplyr::ungroup()` after grouped summaries unless grouped output is
intentional.

## Data Masking

Use `{{ }}` for forwarding bare column arguments inside functions:

```r
summarise_value <- function(data_input, group_column) {
  res_summary <-
    data_input |>
    dplyr::group_by({{ group_column }}) |>
    dplyr::summarise(
      n_records = dplyr::n(),
      .groups = "drop"
    )

  return(res_summary)
}
```

Use `.data[[column_name]]` when the column is stored as a character string.

## Targets Pipelines

Pipeline scripts live in `R/target_pipelines/`. Keep target commands readable
and deterministic.

When a target command becomes multi-step logic, move the logic into a function
under `R/functions/` and call that function from the target.

Use target stores consistently:

| Script | Store |
| --- | --- |
| `01_pipeline_pollen_data.R` | `Targets_data/pipeline_pollen_data` |
| `02_pipeline_paps.R` | `Targets_data/pipeline_paps` |
| `03_pipeline_events.R` | `Targets_data/pipeline_events` |
| `04_pipeline_predictors.R` | `Targets_data/pipeline_predictors` |
| `05_pipeline_hvar_spatial_temporal.R` | `Targets_data/analyses_h1` |
| `06_pipeline_multidimensional_shifts.R` | `Targets_data/analyses_h2` |

Before running expensive targets, prefer `targets::tar_manifest()` or
`targets::tar_visnetwork()` to inspect the pipeline.

## Visualisation

Use constants from `R/00_Config_file.R` for figure style and size:

- `text_size`
- `line_size`
- `point_size`
- `image_width_vec`
- `image_units`
- project palettes

Build plots in this order:

1. `ggplot2::ggplot()`
2. facets
3. scales
4. labels
5. theme calls
6. geoms, from bottom to top layer

Save generated outputs under `Outputs/` or the configured external data path,
following nearby visualisation scripts.

## Reproducibility

- Set seeds explicitly when randomness is used.
- The project setup uses `set_seed <- 1234`.
- Keep generated data and figures traceable to scripts, targets, or configured
  external data.
- Do not rely on interactive R state.
- Do not use environment variables (`Sys.setenv()`, `Sys.getenv()`) to control normal project logic. Treat this as an exceptional last resort.
- Prefer explicit function/script inputs (`flag_*`, arguments, config objects) over hidden global environment switches.
