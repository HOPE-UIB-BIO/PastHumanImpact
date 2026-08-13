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
- `R/temporal_models/` - temporal model preparation, fitting, checks, and
  prediction
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

Never use GitHub issue or pull-request numbers in R identifiers, target names, analysis labels, configuration values, comments, roxygen documentation, or script filenames.
Name code after its scientific meaning or computational role so it remains understandable without access to GitHub.
Issue identifiers may appear in planning or reviewer-response documents whose explicit purpose is traceability, but executable code must not depend on that context.

Prefer creating a new object name for transformed data rather than overwriting an existing object in memory. Reuse an object name only when there is a clear reason (for example memory constraints, tight loops, or deliberate in-place workflow), and keep that choice explicit.

Function names should be verbs. Data objects should be nouns.

## Formatting

- Use `<-` for assignment.
- Use 2-space indentation.
- Keep R code and roxygen lines to about 80 characters. Do not apply this limit to prose in Markdown or Quarto files.
- Use `TRUE` and `FALSE`, not `T` and `F`.
- Prefer one argument per line for multi-argument calls.
- Use explicit argument names where practical.

### Assignment layout

Put the right-hand side on a new line after `<-` by default.
This applies to function calls, vectors and lists, indexing, arithmetic expressions, conditionals, and pipelines.

```r
data_records <-
  readr::read_csv(path_records)

region_levels <-
  c("North America", "Latin America", "Europe", "Asia", "Oceania")

background_step <-
  background_values[[2]] - background_values[[1]]
```

The only same-line exceptions are short direct aliases and simple atomic literals.
Do not use this exception for a function call, indexing operation, calculation, or collection, even when it would fit on one line.

```r
min_age <- 0
flag_rerun <- FALSE
data_records <- data_records_override
```

### Vertical separation

Separate every top-level executable statement within a block with exactly one blank line.
An assignment, standalone function call, pipeline, or complete control-flow construct counts as one statement.
Apply this rule even when two adjacent statements are closely related.

```r
data_records <-
  prepare_records(data_source)

data_summary <-
  summarise_records(data_records)

assertthat::assert_that(
  nrow(data_summary) > 0L,
  msg = "No summary records are available."
)

if (
  isTRUE(use_override)
) {
  data_summary <- data_summary_override
}

write_summary(data_summary)
```

Do not insert blank lines inside a single continued expression merely to separate its arguments, pipeline stages, ggplot layers, or other continuation lines.
Keep syntactically connected clauses such as `} else {` together.

### Control flow

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
- New R package dependencies may be added when they materially improve the
  implementation, but the agent must explain the need and receive explicit
  user approval before installing, recording, or using them in project code.
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

Follow `.ai/analysis-structure.md` for canonical visualisation-script and output paths. Organise generated figures by scientific analysis, never by their current manuscript number or by whether they are presently considered main, supplementary, extended, or extra material.

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

Save generated outputs under `Outputs/` or the configured external data path using stable, descriptive analysis names. Publication assembly may copy selected outputs to numbered manuscript filenames, but analytical scripts must not generate numbered canonical filenames directly.

## Reproducibility

- Set seeds explicitly when randomness is used.
- The project setup uses `set_seed <- 1234`.
- Keep generated data and figures traceable to scripts, targets, or configured
  external data.
- Do not rely on interactive R state.
- Do not use environment variables (`Sys.setenv()`, `Sys.getenv()`) to control normal project logic. Treat this as an exceptional last resort.
- Prefer explicit function/script inputs (`flag_*`, arguments, config objects) over hidden global environment switches.
