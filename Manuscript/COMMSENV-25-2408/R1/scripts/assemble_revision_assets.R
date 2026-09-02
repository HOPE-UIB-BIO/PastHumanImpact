library(here)

source(here::here("R/00_Config_file.R"))

path_project <-
  here::here("Manuscript", "COMMSENV-25-2408", "R1")

result <-
  build_revision_assets(
    registry_path = file.path(
      path_project,
      "evidence",
      "claim-evidence-registry.csv"
    ),
    project_dir = path_project,
    repository_dir = here::here(),
    manifest_path = file.path(
      path_project,
      "evidence",
      "revision-artifact-manifest.csv"
    )
  )

message(
  "Validated and assembled ",
  nrow(result),
  " revision figures."
)
