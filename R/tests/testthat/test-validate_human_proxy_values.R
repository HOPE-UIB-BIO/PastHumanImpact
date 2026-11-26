data_valid_values <-
  tibble::tibble(
    dataset_id = c("a", "a", "a", "a", "a"),
    age_bp = c(500, 500, 500, 2000, 2000),
    proxy = c("kk10", "kk10", "kk10", "kk10", "spd"),
    support = c("cell", "radius_10km", "spd_buffer", "cell", "spd_buffer"),
    radius_km = c(NA, 10, 250, NA, 250),
    value = c(0.1, 0.2, NA, 0.3, 4),
    value_transformed = c(0.1, 0.2, NA, 0.3, 2),
    is_native_layer = c(TRUE, TRUE, TRUE, FALSE, TRUE)
  )

data_valid_windows <-
  tibble::tibble(
    proxy = c("kk10", "spd"),
    age_min_bp = c(500, 2000),
    age_max_bp = c(8000, 8500)
  )

testthat::test_that("validate_human_proxy_values() accepts the contract", {
  testthat::expect_invisible(
    validate_human_proxy_values(
      data_values = data_valid_values,
      data_proxy_windows = data_valid_windows
    )
  )

  res <-
    validate_human_proxy_values(
      data_values = data_valid_values,
      data_proxy_windows = data_valid_windows
    )

  testthat::expect_identical(res, data_valid_values)
})

testthat::test_that("validate_human_proxy_values() rejects duplicate keys", {
  testthat::expect_error(
    validate_human_proxy_values(
      data_values = dplyr::bind_rows(
        data_valid_values,
        dplyr::slice(data_valid_values, 1L)
      ),
      data_proxy_windows = data_valid_windows
    ),
    regexp = "duplicate"
  )
})

testthat::test_that("validate_human_proxy_values() rejects extrapolation", {
  testthat::expect_error(
    validate_human_proxy_values(
      data_values = dplyr::mutate(
        data_valid_values,
        age_bp = dplyr::if_else(
          .data[["proxy"]] == "spd",
          1500,
          .data[["age_bp"]]
        )
      ),
      data_proxy_windows = data_valid_windows
    ),
    regexp = "window"
  )
  testthat::expect_error(
    validate_human_proxy_values(
      data_values = data_valid_values,
      data_proxy_windows = dplyr::filter(
        data_valid_windows,
        .data[["proxy"]] == "kk10"
      )
    ),
    regexp = "window"
  )
})

testthat::test_that("validate_human_proxy_values() checks supports", {
  testthat::expect_error(
    validate_human_proxy_values(
      data_values = dplyr::mutate(
        data_valid_values,
        radius_km = dplyr::if_else(
          .data[["support"]] == "radius_10km",
          25,
          .data[["radius_km"]]
        )
      ),
      data_proxy_windows = data_valid_windows
    ),
    regexp = "support"
  )
  testthat::expect_error(
    validate_human_proxy_values(
      data_values = dplyr::mutate(
        data_valid_values,
        radius_km = dplyr::if_else(
          .data[["support"]] == "cell",
          1,
          .data[["radius_km"]]
        )
      ),
      data_proxy_windows = data_valid_windows
    ),
    regexp = "support"
  )
  testthat::expect_error(
    validate_human_proxy_values(
      data_values = dplyr::mutate(
        data_valid_values,
        support = dplyr::if_else(
          .data[["support"]] == "cell",
          "polygon",
          .data[["support"]]
        )
      ),
      data_proxy_windows = data_valid_windows
    ),
    regexp = "support"
  )
  testthat::expect_error(
    validate_human_proxy_values(
      data_values = dplyr::mutate(
        data_valid_values,
        radius_km = dplyr::if_else(
          .data[["support"]] == "spd_buffer",
          NA_real_,
          .data[["radius_km"]]
        )
      ),
      data_proxy_windows = data_valid_windows
    ),
    regexp = "support"
  )
})

testthat::test_that("validate_human_proxy_values() checks column types", {
  testthat::expect_error(
    validate_human_proxy_values(
      data_values = dplyr::mutate(
        data_valid_values,
        is_native_layer = c(TRUE, NA, TRUE, FALSE, TRUE)
      ),
      data_proxy_windows = data_valid_windows
    ),
    regexp = "contract"
  )
  testthat::expect_error(
    validate_human_proxy_values(
      data_values = dplyr::mutate(
        data_valid_values,
        dataset_id = 1L
      ),
      data_proxy_windows = data_valid_windows
    ),
    regexp = "contract"
  )
  testthat::expect_error(
    validate_human_proxy_values(
      data_values = dplyr::select(data_valid_values, -"value_transformed"),
      data_proxy_windows = data_valid_windows
    ),
    regexp = "contract"
  )
})

testthat::test_that("validate_human_proxy_values() pairs missing values", {
  testthat::expect_error(
    validate_human_proxy_values(
      data_values = dplyr::mutate(
        data_valid_values,
        value_transformed = c(0.1, NA, NA, 0.3, 2)
      ),
      data_proxy_windows = data_valid_windows
    ),
    regexp = "missing"
  )
})
