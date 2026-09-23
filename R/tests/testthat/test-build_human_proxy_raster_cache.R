testthat::test_that("build_human_proxy_raster_cache() selects layers", {
  source_path <- tempfile(fileext = ".tif")
  cache_path <- tempfile(fileext = ".tif")
  on.exit(unlink(c(source_path, cache_path)), add = TRUE)

  raster <-
    terra::rast(
      nrows = 2,
      ncols = 2,
      nlyrs = 3,
      xmin = 0,
      xmax = 2,
      ymin = 0,
      ymax = 2,
      crs = "EPSG:4326"
    )
  terra::values(raster) <-
    cbind(c(1, 2, 3, 4), c(5, 6, 7, 8), c(9, 10, 11, 12))
  terra::writeRaster(raster, source_path, overwrite = TRUE)

  result <-
    build_human_proxy_raster_cache(
      file_path = source_path,
      source_id = "test",
      expected_layers = 3L,
      layer_indices = c(3L, 1L),
      cache_path = cache_path
    )

  cached <- terra::rast(result)

  testthat::expect_true(file.exists(result))
  testthat::expect_equal(terra::nlyr(cached), 2L)
  testthat::expect_equal(
    unname(terra::values(cached)),
    cbind(c(9, 10, 11, 12), c(1, 2, 3, 4))
  )
})
