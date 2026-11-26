testthat::test_that("prepare_human_proxy_support_locations() builds supports", {
  data_meta <-
    tibble::tibble(
      dataset_id = c("b", "a"),
      long = c(10, 20),
      lat = c(-5, 45)
    )

  data_spd_support <-
    tibble::tibble(
      dataset_id = c("a", "z"),
      radius_km = c(250, 500)
    )

  res <-
    prepare_human_proxy_support_locations(
      data_meta = data_meta,
      data_spd_support = data_spd_support,
      radii_km = c(25, 10)
    )

  testthat::expect_named(
    res,
    c(
      "location_id",
      "dataset_id",
      "support",
      "radius_km",
      "extraction",
      "long",
      "lat"
    )
  )
  testthat::expect_equal(base::nrow(res), 7L)
  testthat::expect_identical(
    dplyr::pull(res, "support"),
    c(
      "cell",
      "radius_10km",
      "radius_25km",
      "spd_buffer",
      "cell",
      "radius_10km",
      "radius_25km"
    )
  )
  testthat::expect_identical(
    dplyr::pull(res, "dataset_id"),
    c("a", "a", "a", "a", "b", "b", "b")
  )
  testthat::expect_equal(
    dplyr::pull(res, "radius_km"),
    c(NA, 10, 25, 250, NA, 10, 25)
  )
  testthat::expect_identical(
    dplyr::pull(res, "extraction"),
    c("point", "buffer", "buffer", "buffer", "point", "buffer", "buffer")
  )
  testthat::expect_identical(
    dplyr::pull(res, "location_id")[[4]],
    "a__spd_buffer"
  )
  testthat::expect_false(
    base::anyDuplicated(dplyr::pull(res, "location_id")) > 0L
  )
  testthat::expect_equal(dplyr::pull(res, "lat")[[5]], -5)
})

testthat::test_that("prepare_human_proxy_support_locations() takes ids", {
  res <-
    prepare_human_proxy_support_locations(
      data_meta = tibble::tibble(dataset_id = 7L, long = 1, lat = 2),
      data_spd_support = tibble::tibble(dataset_id = "7", radius_km = 500),
      radii_km = 50
    )

  testthat::expect_type(dplyr::pull(res, "dataset_id"), "character")
  testthat::expect_identical(
    dplyr::pull(res, "support"),
    c("cell", "radius_50km", "spd_buffer")
  )
})

testthat::test_that("prepare_human_proxy_support_locations() validates", {
  data_spd_support <-
    tibble::tibble(dataset_id = "a", radius_km = 250)

  testthat::expect_error(
    prepare_human_proxy_support_locations(
      data_meta = tibble::tibble(dataset_id = "a", long = 1),
      data_spd_support = data_spd_support
    ),
    regexp = "contract"
  )
  testthat::expect_error(
    prepare_human_proxy_support_locations(
      data_meta = tibble::tibble(dataset_id = "a", long = NA_real_, lat = 1),
      data_spd_support = data_spd_support
    ),
    regexp = "contract"
  )
  testthat::expect_error(
    prepare_human_proxy_support_locations(
      data_meta = tibble::tibble(
        dataset_id = c("a", "a"),
        long = 1,
        lat = 1
      ),
      data_spd_support = data_spd_support
    ),
    regexp = "contract"
  )
  testthat::expect_error(
    prepare_human_proxy_support_locations(
      data_meta = tibble::tibble(dataset_id = "a", long = 1, lat = 1),
      data_spd_support = data_spd_support,
      radii_km = c(10, -5)
    ),
    regexp = "contract"
  )
  testthat::expect_error(
    prepare_human_proxy_support_locations(
      data_meta = tibble::tibble(dataset_id = "a", long = 1, lat = 1),
      data_spd_support = data_spd_support,
      radii_km = c(10, 10)
    ),
    regexp = "contract"
  )
  testthat::expect_error(
    prepare_human_proxy_support_locations(
      data_meta = tibble::tibble(dataset_id = "a", long = 1, lat = 1),
      data_spd_support = tibble::tibble(dataset_id = "a", radius_km = 0)
    ),
    regexp = "contract"
  )
})
