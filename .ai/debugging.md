# Debugging Guidance

Canonical debugging workflow guidance for this repository.

## Guiding Principle

Reproduce and understand the root cause before editing project files. Prefer the
smallest change that fixes the confirmed problem.

## Workflow

### 1. Reproduce in a Temporary Script

Create a throwaway script in `Data/Temp/`:

```r
# Data/Temp/debug_<topic>.R

library(<pkg>)

# Minimal reproducible example.
```

Keep the script self-contained. Do not rely on variables left in an interactive
R session.

### 2. Run in a Clean Terminal

Run the reproduction with `Rscript` from the project root:

```powershell
Rscript "Data/Temp/debug_<topic>.R"
```

If PowerShell output is garbled or truncated, redirect to a temporary text file
and inspect that file.

### 3. Load Project Context Only When Needed

For project-level bugs, source the project configuration explicitly:

```r
library(here)

source(
  here::here("R/00_Config_file.R")
)
```

`R/00_Config_file.R` restores `renv`, loads packages, sources functions under
`R/functions/`, sets core constants, and resolves `data_storage_path`.

### 4. Apply the Fix

Once the root cause is confirmed:

- edit the smallest relevant source file
- add a concise comment only when the fix depends on non-obvious behavior
- remove all temporary debug scripts and output files before finishing

### 5. Validate

Use the narrowest reliable check first, then broaden as needed:

```r
library(here)

source(
  here::here("R/00_Config_file.R")
)
```

For a changed function, run or add a focused test if a test file exists for that
function. If no test harness exists yet, run a small script that exercises the
changed function with representative input.

For target pipelines, verify the affected pipeline manifest:

```r
targets::tar_manifest(
  script = here::here("R/target_pipelines/<pipeline>.R"),
  store = file.path(data_storage_path, "Targets_data/<store_name>")
)
```

Run full pipeline stages only when the change affects shared functions,
pipeline contracts, or analysis outputs:

```r
targets::tar_make(
  script = here::here("R/target_pipelines/01_pipeline_pollen_data.R"),
  store = file.path(data_storage_path, "Targets_data/pipeline_pollen_data")
)
```

The complete project workflow is `R/01_run_project.R`, but it is expensive and
depends on external data. Do not run it casually; run it when the task requires
end-to-end verification.

## Common Pitfalls

- Do not trust interactive R state for a reproduction.
- Do not batch unrelated hypotheses in one debug script.
- Remember that the repository does not include all data. Missing external data
  is an environment issue, not necessarily a code regression.
- On Windows, inspect R output text before treating a non-zero exit code caused
  by warnings as a failed analysis.
