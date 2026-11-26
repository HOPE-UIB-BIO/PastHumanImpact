testthat::test_that("save_hyde_land_use_fraction_cache() computes fractions", {
  path_dir <-
    base::tempfile("hyde-cache-")

  base::dir.create(path_dir)

  raster_template <-
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

  raster_cropland <- raster_template

  terra::values(raster_cropland) <-
    base::cbind(c(1, 2, 0, NA), c(2, 4, 0, 1), c(0, 0, 0, 0))

  raster_grazing <- raster_template

  terra::values(raster_grazing) <-
    base::cbind(c(1, 0, 0, 1), c(2, 4, 8, 1), c(0, 0, 0, 0))

  raster_area <-
    terra::rast(
      nrows = 2,
      ncols = 2,
      xmin = 0,
      xmax = 2,
      ymin = 0,
      ymax = 2,
      crs = "EPSG:4326"
    )

  terra::values(raster_area) <- c(4, 8, 8, 2)

  vec_paths <-
    file.path(path_dir, c("crop.tif", "graze.tif", "area.tif"))

  terra::writeRaster(raster_cropland, vec_paths[[1]])

  terra::writeRaster(raster_grazing, vec_paths[[2]])

  terra::writeRaster(raster_area, vec_paths[[3]])

  path_cache <-
    file.path(path_dir, "cache", "land_use.tif")

  res <-
    save_hyde_land_use_fraction_cache(
      path_cropland = vec_paths[[1]],
      path_grazing = vec_paths[[2]],
      path_cell_area = vec_paths[[3]],
      layer_indices = c(2L, 1L),
      cache_path = path_cache
    )

  testthat::expect_true(base::file.exists(res))

  raster_result <-
    terra::rast(res)

  testthat::expect_equal(terra::nlyr(raster_result), 2L)

  mat_values <-
    terra::values(raster_result)

  testthat::expect_equal(
    base::unname(mat_values[, 1]),
    c(1, 1, 1, 1)
  )
  testthat::expect_equal(
    base::unname(mat_values[, 2]),
    c(0.5, 0.25, 0, NA)
  )
})

testthat::test_that("save_hyde_land_use_fraction_cache() checks grids", {
  path_dir <-
    base::tempfile("hyde-cache-")

  base::dir.create(path_dir)

  raster_two <-
    terra::rast(
      nrows = 2,
      ncols = 2,
      nlyrs = 2,
      xmin = 0,
      xmax = 2,
      ymin = 0,
      ymax = 2,
      crs = "EPSG:4326"
    )

  terra::values(raster_two) <- 1

  raster_shifted <-
    terra::rast(
      nrows = 2,
      ncols = 2,
      xmin = 1,
      xmax = 3,
      ymin = 0,
      ymax = 2,
      crs = "EPSG:4326"
    )

  terra::values(raster_shifted) <- 4

  raster_three <-
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

  terra::values(raster_three) <- 1

  raster_area <-
    terra::rast(
      nrows = 2,
      ncols = 2,
      xmin = 0,
      xmax = 2,
      ymin = 0,
      ymax = 2,
      crs = "EPSG:4326"
    )

  terra::values(raster_area) <- 4

  vec_paths <-
    file.path(
      path_dir,
      c("two.tif", "shifted.tif", "three.tif", "area.tif")
    )

  terra::writeRaster(raster_two, vec_paths[[1]])

  terra::writeRaster(raster_shifted, vec_paths[[2]])

  terra::writeRaster(raster_three, vec_paths[[3]])

  terra::writeRaster(raster_area, vec_paths[[4]])

  testthat::expect_error(
    save_hyde_land_use_fraction_cache(
      path_cropland = vec_paths[[1]],
      path_grazing = vec_paths[[1]],
      path_cell_area = vec_paths[[2]],
      layer_indices = 1L,
      cache_path = file.path(path_dir, "out.tif")
    ),
    regexp = "grid"
  )
  testthat::expect_error(
    save_hyde_land_use_fraction_cache(
      path_cropland = vec_paths[[1]],
      path_grazing = vec_paths[[3]],
      path_cell_area = vec_paths[[1]],
      layer_indices = 1L,
      cache_path = file.path(path_dir, "out.tif")
    ),
    regexp = "grid"
  )
  testthat::expect_error(
    save_hyde_land_use_fraction_cache(
      path_cropland = vec_paths[[1]],
      path_grazing = vec_paths[[1]],
      path_cell_area = vec_paths[[4]],
      layer_indices = 5L,
      cache_path = file.path(path_dir, "out.tif")
    ),
    regexp = "contract"
  )
  testthat::expect_error(
    save_hyde_land_use_fraction_cache(
      path_cropland = vec_paths[[1]],
      path_grazing = vec_paths[[1]],
      path_cell_area = vec_paths[[1]],
      layer_indices = 1L,
      cache_path = file.path(path_dir, "out.nc")
    ),
    regexp = "contract"
  )
})
