# Pollen-derived property preparation

## `mvpart` compatibility boundary

The `data_mrt` and `data_change_points` targets use the archived `mvpart` 1.6-2 implementation directly. The package compiles and runs with R 3.5.0, but it does not compile with R 4.3.0 or newer because its C source uses the removed `Sint` type.

Do not start this complete targets pipeline with R 3.5.0. The pipeline sources `R/00_Config_file.R`, restores the current project lockfile, and loads the modern project package set. Those packages are neither needed nor guaranteed to support R 3.5.0.

Resolve the installer from the repository root, then run it from a neutral working directory. This prevents the repository `.Rprofile` from activating the modern `renv` environment before the installer starts:

```powershell
$script = (Resolve-Path "R/analyses/01_data_preparation/05_paps/install_mvpart_runtime.R").Path
Push-Location ([System.IO.Path]::GetTempPath())
rig run --r-version 3.5.0 --script $script
Pop-Location
```

On Windows, source installation also requires the Rtools toolchain compatible with R 3.5. The installer is intentionally a base-R script: it does not source the project configuration, activate `renv`, or install `remotes`, `REcopol`, `vegan`, or the tidyverse. It installs pinned versions of `mvpart` 1.6-2, `purrr` 0.3.4, `rlang` 0.4.11, `magrittr` 2.0.1, and `assertthat` 0.2.1 into `~/R/legacy-library/mvpart-r35` by default. Supply one trailing path argument to use another external library.

The old-R runner uses `{purrr}` to satisfy the project iteration rules. Its only contributed runtime packages are `mvpart`, `purrr`, the two direct `purrr` dependencies `rlang` and `magrittr`, and `assertthat` for function contracts. `survival` is suggested by `mvpart` but is not loaded by the multivariate or univariate operations used here.

## Random seed

Both operations prune their trees by cross-validation, which assigns samples to folds at random, so the change points depend on the state of the random-number generator. The `{targets}` seed does not reach the R 3.5 subprocess. The pipeline therefore passes the project seed (`set_seed` in `R/00_Config_file.R`) to `run_mvpart_runtime()`, which writes it into the request. The runner passes it to `compute_mrt()` and `compute_pap_change_points()`, which reset the generator to that seed before each dataset. The result for a dataset therefore depends only on its own data and the seed, not on the order or number of datasets in the run. The seed is recorded in `metadata_mrt_runtime` and `metadata_change_point_runtime`, and `validate_mvpart_runtime_result()` rejects a result that does not record the requested seed.

Results are reproducible only within the pinned R 3.5.0 runtime. R 3.6.0 changed the default algorithm of `sample()`, so the same seed gives different cross-validation folds in later R versions.

## Minimal production design

Keep the old-R process at a serialized-data boundary:

1. The modern pipeline prepares and validates the input, then writes an RDS.
2. A small R 3.5 runner prepends the isolated library, reads the RDS, and calls `mvpart` directly.
3. The runner writes a base R result RDS for the modern pipeline to validate and consume.

The old-R runner should reproduce the small transformations currently wrapped by `REcopol` with base R. In particular, the chi-square transformation is `sqrt(sum(x)) * x / outer(rowSums(x), sqrt(colSums(x)))`. This avoids pulling the full `REcopol` and `vegan` dependency trees into the legacy library.

The modern targets graph invokes the old-R runner only for `data_mrt` and `data_change_points`. All input preparation, target tracking, validation, and downstream analysis remain under the project R version.
