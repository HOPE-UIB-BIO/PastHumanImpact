testthat::test_that("load_human_proxy_raster() loads a geographic raster", {
  path_raster <-
    tempfile(fileext = ".tif")

  raster <-
    terra::rast(
      nrows = 2,
      ncols = 2,
      nlyrs = 2,
      xmin = -1,
      xmax = 1,
      ymin = -1,
      ymax = 1,
      crs = "EPSG:4326"
    )

  terra::values(raster) <- seq_len(8)

  terra::writeRaster(raster, path_raster, overwrite = TRUE)

  result <-
    load_human_proxy_raster(path_raster, "test", 2L)

  testthat::expect_s4_class(result, "SpatRaster")
  testthat::expect_equal(terra::nlyr(result), 2L)
})

testthat::test_that("load_human_proxy_raster() validates layer count", {
  path_raster <-
    tempfile(fileext = ".tif")

  raster <-
    terra::rast(nrows = 1, ncols = 1, crs = "EPSG:4326")

  terra::values(raster) <- 1

  terra::writeRaster(raster, path_raster, overwrite = TRUE)

  testthat::expect_error(
    load_human_proxy_raster(path_raster, "test", 2L),
    regexp = "contains 1 layers"
  )
})
