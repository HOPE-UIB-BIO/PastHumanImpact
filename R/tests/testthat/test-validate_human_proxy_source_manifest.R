testthat::test_that("validate_human_proxy_source_manifest() resolves files", {
  path_root <-
    tempfile("proxy-sources-")

  dir.create(path_root)

  file.create(file.path(path_root, c("kk10.nc", "popc.tif")))

  sources <-
    tibble::tibble(
      source_id = c("kk10", "hyde_3_2"),
      product = c("KK10", "HYDE"),
      version = c("2011", "3.2"),
      variable = c("land_use", "popc"),
      units = c("fraction", "people per cell"),
      file_relative_path = c("kk10.nc", "popc.tif"),
      source_url = c("https://example.org/a", "https://example.org/b"),
      download_url = c(
        "https://example.org/a.bin",
        "https://example.org/b.zip"
      ),
      doi = c("doi:a", "doi:b"),
      license = c("CC-BY-3.0", "source terms"),
      expected_layers = c(7901L, 75L),
      aggregation = c("area_weighted_mean", "cell_center_sum")
    )

  result <-
    validate_human_proxy_source_manifest(sources, path_root)

  testthat::expect_true(all(file.exists(result[["file_path"]])))
})

testthat::test_that("validate_human_proxy_source_manifest() reports files", {
  sources <-
    tibble::tibble(
      source_id = c("kk10", "hyde_3_2"),
      product = c("KK10", "HYDE"),
      version = c("2011", "3.2"),
      variable = c("land_use", "popc"),
      units = c("fraction", "people per cell"),
      file_relative_path = c("kk10.nc", "popc.tif"),
      source_url = c("https://example.org/a", "https://example.org/b"),
      download_url = c(
        "https://example.org/a.bin",
        "https://example.org/b.zip"
      ),
      doi = c("doi:a", "doi:b"),
      license = c("CC-BY-3.0", "source terms"),
      expected_layers = c(7901L, 75L),
      aggregation = c("area_weighted_mean", "cell_center_sum")
    )

  testthat::expect_error(
    validate_human_proxy_source_manifest(sources, tempdir()),
    regexp = "download_sources[.]R"
  )
})
