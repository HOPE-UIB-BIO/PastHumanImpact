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

testthat::test_that("validate_human_proxy_source_manifest() takes source ids", {
  path_root <-
    tempfile("proxy-sources-")

  dir.create(path_root)

  base::writeLines("abc", file.path(path_root, "a.nc"))

  base::writeLines("abcdef", file.path(path_root, "b.nc"))

  base::writeLines("abcdefghi", file.path(path_root, "c.asc"))

  sources <-
    tibble::tibble(
      source_id = c("kk10", "hyde_3_5_cropland", "hyde_3_5_cell_area"),
      product = c("KK10", "HYDE", "HYDE"),
      version = c("2011", "3.5", "3.5"),
      variable = c("land_use", "cropland", "garea"),
      units = c("fraction", "km2 per cell", "km2"),
      file_relative_path = c("a.nc", "b.nc", "c.asc"),
      source_url = "https://example.org/a",
      download_url = "https://example.org/a.bin",
      doi = "doi:a",
      license = "CC-BY-4.0",
      expected_layers = c(7901L, 26L, 1L),
      aggregation = "area_weighted_mean",
      file_size_bytes = base::file.size(
        file.path(path_root, c("a.nc", "b.nc", "c.asc"))
      )
    )

  result <-
    validate_human_proxy_source_manifest(
      data_sources = sources,
      data_storage_path = path_root,
      expected_source_ids = c(
        "kk10",
        "hyde_3_5_cropland",
        "hyde_3_5_cell_area"
      )
    )

  testthat::expect_equal(base::nrow(result), 3L)
  testthat::expect_true(
    base::all(base::file.exists(dplyr::pull(result, "file_path")))
  )
})

testthat::test_that("validate_human_proxy_source_manifest() checks ids", {
  sources <-
    tibble::tibble(
      source_id = c("kk10", "hyde_3_2"),
      product = "P",
      version = "1",
      variable = "v",
      units = "u",
      file_relative_path = c("a.nc", "b.nc"),
      source_url = "https://example.org/a",
      download_url = "https://example.org/a.bin",
      doi = "doi:a",
      license = "CC-BY-4.0",
      expected_layers = 1L,
      aggregation = "area_weighted_mean"
    )

  testthat::expect_error(
    validate_human_proxy_source_manifest(
      data_sources = sources,
      data_storage_path = base::tempdir(),
      require_files = FALSE,
      expected_source_ids = c("kk10", "hyde_3_5_grazing")
    ),
    regexp = "does not satisfy its contract"
  )
})

testthat::test_that("validate_human_proxy_source_manifest() checks sizes", {
  path_root <-
    tempfile("proxy-sources-")

  dir.create(path_root)

  base::writeLines("abc", file.path(path_root, "a.nc"))

  sources <-
    tibble::tibble(
      source_id = "kk10",
      product = "KK10",
      version = "2011",
      variable = "land_use",
      units = "fraction",
      file_relative_path = "a.nc",
      source_url = "https://example.org/a",
      download_url = "https://example.org/a.bin",
      doi = "doi:a",
      license = "CC-BY-3.0",
      expected_layers = 7901L,
      aggregation = "area_weighted_mean",
      file_size_bytes = 999
    )

  testthat::expect_error(
    validate_human_proxy_source_manifest(
      data_sources = sources,
      data_storage_path = path_root,
      expected_source_ids = "kk10"
    ),
    regexp = "size"
  )
})

testthat::test_that("validate_human_proxy_source_manifest() shows hints", {
  sources <-
    tibble::tibble(
      source_id = "kk10",
      product = "KK10",
      version = "2011",
      variable = "land_use",
      units = "fraction",
      file_relative_path = "missing.nc",
      source_url = "https://example.org/a",
      download_url = "https://example.org/a.bin",
      doi = "doi:a",
      license = "CC-BY-3.0",
      expected_layers = 7901L,
      aggregation = "area_weighted_mean"
    )

  testthat::expect_error(
    validate_human_proxy_source_manifest(
      data_sources = sources,
      data_storage_path = base::tempdir(),
      expected_source_ids = "kk10",
      acquisition_hint = "Download it by hand from the archive."
    ),
    regexp = "by hand"
  )
})
