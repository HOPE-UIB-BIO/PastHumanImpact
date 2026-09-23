#----------------------------------------------------------#
# Download external human-impact proxy rasters
#----------------------------------------------------------#

library(here)

source(here::here("R/00_Config_file.R"))

arguments <- commandArgs(trailingOnly = TRUE)

if (
  any(!arguments %in% "--overwrite")
) {
  cli::cli_abort("Supported argument: `--overwrite`.")
}

overwrite_sources <- "--overwrite" %in% arguments

path_manifest <-
  here::here(
    "R",
    "analyses",
    "91_sensitivity_analyses",
    "human_proxy_convergence",
    "proxy_sources.csv"
  )

data_sources <-
  readr::read_csv(path_manifest, show_col_types = FALSE)

table_acquired_sources <-
  run_human_proxy_source_acquisition(
    data_sources = data_sources,
    data_storage_path = data_storage_path,
    overwrite = overwrite_sources,
    validate_rasters = TRUE
  )

print(
  table_acquired_sources |>
    dplyr::select(
      dplyr::all_of(c("source_id", "file_path", "file_size_bytes"))
    )
)
