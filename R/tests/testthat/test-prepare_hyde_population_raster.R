testthat::test_that("prepare_hyde_population_raster() extracts nested file", {
  path_root <- tempfile("hyde-nested-")
  dir.create(path_root)

  path_population_root <- file.path(path_root, "population")
  dir.create(path_population_root)
  writeLines("synthetic raster", file.path(path_population_root, "popc.tif"))

  path_population_zip <- file.path(path_root, "popc.tif.zip")
  zip::zipr(
    zipfile = path_population_zip,
    files = "popc.tif",
    root = path_population_root
  )

  path_hyde_root <- file.path(path_root, "hyde-root", "HYDE")
  dir.create(path_hyde_root, recursive = TRUE)
  file.copy(path_population_zip, file.path(path_hyde_root, "popc.tif.zip"))

  path_hyde_zip <- file.path(path_root, "HYDE.zip")
  zip::zipr(
    zipfile = path_hyde_zip,
    files = "HYDE/popc.tif.zip",
    root = file.path(path_root, "hyde-root")
  )

  path_raw_root <- file.path(path_root, "raw-root", "raw-data")
  dir.create(path_raw_root, recursive = TRUE)
  file.copy(path_hyde_zip, file.path(path_raw_root, "HYDE.zip"))

  path_raw_zip <- file.path(path_root, "raw-data.zip")
  zip::zipr(
    zipfile = path_raw_zip,
    files = "raw-data/HYDE.zip",
    root = file.path(path_root, "raw-root")
  )

  destination <- file.path(path_root, "output", "popc.tif")
  result <- prepare_hyde_population_raster(path_raw_zip, destination)

  testthat::expect_true(file.exists(result))
  testthat::expect_identical(readLines(result), "synthetic raster")
})
