testthat::test_that("aggregate_human_proxy_raster() extracts point values", {
  raster <-
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

  terra::values(raster) <- cbind(c(1, 2, 3, 4), c(5, 6, 7, 8))

  locations <-
    tibble::tibble(
      dataset_id = "a",
      long = 0.5,
      lat = 0.5,
      radius_km = 250
    )

  layers <-
    tibble::tibble(layer_index = 1:2, age_bp = c(1000, 500))

  result <-
    aggregate_human_proxy_raster(
      raster_source = raster,
      data_locations = locations,
      data_layers = layers,
      proxy = "test",
      extraction = "point"
    )

  testthat::expect_identical(result[["age_bp"]], c(1000, 500))
  testthat::expect_equal(result[["value"]], c(3, 7))
})

testthat::test_that("buffer extraction area-weights raster cells", {
  raster <-
    terra::rast(
      nrows = 2,
      ncols = 2,
      xmin = -1,
      xmax = 1,
      ymin = -1,
      ymax = 1,
      crs = "EPSG:4326"
    )
  terra::values(raster) <- c(1, 2, 3, 4)

  result <-
    aggregate_human_proxy_raster(
      raster_source = raster,
      data_locations = tibble::tibble(
        dataset_id = "a",
        long = 0,
        lat = 0,
        radius_km = 150
      ),
      data_layers = tibble::tibble(layer_index = 1L, age_bp = 1000),
      proxy = "test",
      aggregation = "area_weighted_mean",
      extraction = "buffer"
    )

  testthat::expect_equal(result[["value"]], 2.5, tolerance = 0.02)
  testthat::expect_identical(result[["n_cells"]], 4L)
  testthat::expect_true(result[["coverage_weight"]] > 0)
})
testthat::test_that("buffer extraction preserves IDs across chunks", {
  raster <-
    terra::rast(
      nrows = 2,
      ncols = 2,
      xmin = -1,
      xmax = 1,
      ymin = -1,
      ymax = 1,
      crs = "EPSG:4326"
    )
  terra::values(raster) <- c(1, 2, 3, 4)

  locations <-
    tibble::tibble(
      dataset_id = stringr::str_c("record_", seq_len(26)),
      long = 0,
      lat = 0,
      radius_km = 150
    )

  result <-
    aggregate_human_proxy_raster(
      raster_source = raster,
      data_locations = locations,
      data_layers = tibble::tibble(layer_index = 1L, age_bp = 1000),
      proxy = "test",
      aggregation = "area_weighted_mean",
      extraction = "buffer"
    )

  testthat::expect_setequal(
    result[["dataset_id"]],
    locations[["dataset_id"]]
  )
  testthat::expect_equal(result[["value"]], rep(2.5, 26), tolerance = 0.02)
})