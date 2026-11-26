testthat::test_that("aggregate_human_proxy_supports() gives hand means", {
  raster_source <-
    terra::rast(
      nrows = 160,
      ncols = 160,
      nlyrs = 2,
      xmin = -4,
      xmax = 4,
      ymin = -4,
      ymax = 4,
      crs = "EPSG:4326"
    )

  mat_xy <-
    terra::xyFromCell(raster_source, base::seq_len(terra::ncell(raster_source)))

  terra::values(raster_source) <-
    base::cbind(
      base::as.numeric(mat_xy[, 1] > 0),
      base::rep(3, terra::ncell(raster_source))
    )

  data_locations <-
    tibble::tibble(
      location_id = c("a__cell", "a__radius_10km", "a__spd_buffer"),
      dataset_id = "a",
      support = c("cell", "radius_10km", "spd_buffer"),
      radius_km = c(NA, 10, 250),
      extraction = c("point", "buffer", "buffer"),
      long = c(0.01, 0, 0),
      lat = c(0.01, 0, 0)
    )

  res <-
    base::suppressMessages(
      aggregate_human_proxy_supports(
        raster_source = raster_source,
        data_support_locations = data_locations,
        data_layers = tibble::tibble(
          layer_index = c(1L, 2L),
          age_bp = c(1000, 500)
        ),
        proxy = "test"
      )
    ) |>
    dplyr::arrange(.data[["support"]], .data[["age_bp"]])

  testthat::expect_named(
    res,
    c(
      "dataset_id",
      "support",
      "radius_km",
      "age_bp",
      "proxy",
      "value",
      "n_cells",
      "coverage_weight"
    ),
    ignore.order = TRUE
  )
  testthat::expect_equal(base::nrow(res), 6L)

  data_cell <-
    res |>
    dplyr::filter(.data[["support"]] == "cell")

  testthat::expect_equal(dplyr::pull(data_cell, "value"), c(3, 1))
  testthat::expect_true(base::all(is.na(dplyr::pull(data_cell, "radius_km"))))

  data_radius <-
    res |>
    dplyr::filter(.data[["support"]] == "radius_10km")

  testthat::expect_equal(dplyr::pull(data_radius, "value"), c(3, 0.5),
    tolerance = 0.02
  )
  testthat::expect_equal(dplyr::pull(data_radius, "radius_km"), c(10, 10))

  data_spd <-
    res |>
    dplyr::filter(.data[["support"]] == "spd_buffer")

  testthat::expect_equal(dplyr::pull(data_spd, "value"), c(3, 0.5),
    tolerance = 0.02
  )
  testthat::expect_equal(dplyr::pull(data_spd, "radius_km"), c(250, 250))
  testthat::expect_true(base::all(dplyr::pull(res, "proxy") == "test"))
  testthat::expect_true(
    dplyr::pull(data_spd, "n_cells")[[1]] >
      dplyr::pull(data_radius, "n_cells")[[1]]
  )
})

testthat::test_that("aggregate_human_proxy_supports() keeps records apart", {
  raster_source <-
    terra::rast(
      nrows = 40,
      ncols = 80,
      xmin = -4,
      xmax = 4,
      ymin = -2,
      ymax = 2,
      crs = "EPSG:4326"
    )

  mat_xy <-
    terra::xyFromCell(raster_source, base::seq_len(terra::ncell(raster_source)))

  terra::values(raster_source) <-
    dplyr::if_else(mat_xy[, 1] > 0, 7, 2)

  data_locations <-
    tibble::tibble(
      location_id = c("east__cell", "east__radius_25km", "west__cell"),
      dataset_id = c("east", "east", "west"),
      support = c("cell", "radius_25km", "cell"),
      radius_km = c(NA, 25, NA),
      extraction = c("point", "buffer", "point"),
      long = c(2, 2, -2),
      lat = c(0, 0, 0)
    )

  res <-
    base::suppressMessages(
      aggregate_human_proxy_supports(
        raster_source = raster_source,
        data_support_locations = data_locations,
        data_layers = tibble::tibble(layer_index = 1L, age_bp = 2000),
        proxy = "test"
      )
    ) |>
    dplyr::arrange(.data[["dataset_id"]], .data[["support"]])

  testthat::expect_identical(
    dplyr::pull(res, "dataset_id"),
    c("east", "east", "west")
  )
  testthat::expect_equal(dplyr::pull(res, "value"), c(7, 7, 2))
})

testthat::test_that("aggregate_human_proxy_supports() keeps missing cells", {
  raster_source <-
    terra::rast(
      nrows = 2,
      ncols = 2,
      xmin = 0,
      xmax = 2,
      ymin = 0,
      ymax = 2,
      crs = "EPSG:4326"
    )

  terra::values(raster_source) <- c(NA, 1, 1, 1)

  res <-
    aggregate_human_proxy_supports(
      raster_source = raster_source,
      data_support_locations = tibble::tibble(
        location_id = "a__cell",
        dataset_id = "a",
        support = "cell",
        radius_km = NA_real_,
        extraction = "point",
        long = 0.5,
        lat = 1.5
      ),
      data_layers = tibble::tibble(layer_index = 1L, age_bp = 100),
      proxy = "test"
    )

  testthat::expect_equal(base::nrow(res), 1L)
  testthat::expect_true(is.na(dplyr::pull(res, "value")))
})

testthat::test_that("aggregate_human_proxy_supports() validates inputs", {
  raster_source <-
    terra::rast(
      nrows = 2,
      ncols = 2,
      xmin = 0,
      xmax = 2,
      ymin = 0,
      ymax = 2,
      crs = "EPSG:4326"
    )

  terra::values(raster_source) <- 1

  testthat::expect_error(
    aggregate_human_proxy_supports(
      raster_source = raster_source,
      data_support_locations = tibble::tibble(
        location_id = "a__cell",
        dataset_id = "a",
        support = "cell",
        long = 0.5,
        lat = 0.5
      ),
      data_layers = tibble::tibble(layer_index = 1L, age_bp = 100),
      proxy = "test"
    ),
    regexp = "contract"
  )
  testthat::expect_error(
    aggregate_human_proxy_supports(
      raster_source = raster_source,
      data_support_locations = tibble::tibble(
        location_id = "a__cell",
        dataset_id = "a",
        support = "cell",
        radius_km = NA_real_,
        extraction = "polygon",
        long = 0.5,
        lat = 0.5
      ),
      data_layers = tibble::tibble(layer_index = 1L, age_bp = 100),
      proxy = "test"
    ),
    regexp = "contract"
  )
})
