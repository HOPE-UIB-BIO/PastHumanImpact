testthat::test_that("build_revision_assets() copies only registered files", {
  repository_dir <-
    tempfile("revision-repository-")

  project_dir <-
    file.path(repository_dir, "Manuscript", "COMMSENV-25-2408", "R1")

  dir.create(
    file.path(repository_dir, "Outputs", "Figures"),
    recursive = TRUE
  )

  dir.create(
    file.path(project_dir, "figures"),
    recursive = TRUE
  )

  source_path <-
    file.path(repository_dir, "Outputs", "Figures", "source.pdf")

  writeLines("figure", source_path)

  registry_path <-
    file.path(project_dir, "registry.csv")

  readr::write_csv(
    tibble::tibble(
      artifact_id = "figure_a",
      path = "Outputs/Figures/source.pdf",
      figure_destination = "figures/figure-a.pdf",
      validation_status = "registered"
    ),
    registry_path
  )

  manifest_path <-
    file.path(project_dir, "evidence", "manifest.csv")

  result <-
    build_revision_assets(
      registry_path = registry_path,
      project_dir = project_dir,
      repository_dir = repository_dir,
      manifest_path = manifest_path
    )

  testthat::expect_identical(
    result[["validation_status"]],
    "validated"
  )

  testthat::expect_true(
    file.exists(file.path(project_dir, "figures", "figure-a.pdf"))
  )

  testthat::expect_true(file.exists(manifest_path))
})
