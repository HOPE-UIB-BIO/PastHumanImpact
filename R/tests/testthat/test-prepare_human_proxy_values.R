testthat::test_that("prepare_human_proxy_values() returns the contract", {
  data_source <-
    tibble::tibble(
      dataset_id = c(2L, 1L, 1L),
      support = c("spd_buffer", "spd_buffer", "spd_buffer"),
      radius_km = c(500, 250, 250),
      age_bp = c(2000, 2000, 2500),
      value = c(4, 9, NA),
      extra_column = "dropped"
    )

  res <-
    prepare_human_proxy_values(
      data_source = data_source,
      proxy = "spd",
      native_ages_bp = c(2000, 3000),
      transformation = "square_root"
    )

  testthat::expect_named(
    res,
    c(
      "dataset_id",
      "age_bp",
      "proxy",
      "support",
      "radius_km",
      "value",
      "value_transformed",
      "is_native_layer"
    )
  )
  testthat::expect_identical(dplyr::pull(res, "dataset_id"), c("1", "1", "2"))
  testthat::expect_equal(dplyr::pull(res, "age_bp"), c(2500, 2000, 2000))
  testthat::expect_equal(dplyr::pull(res, "value_transformed"), c(NA, 3, 2))
  testthat::expect_identical(
    dplyr::pull(res, "is_native_layer"),
    c(FALSE, TRUE, TRUE)
  )
  testthat::expect_true(base::all(dplyr::pull(res, "proxy") == "spd"))
})

testthat::test_that("prepare_human_proxy_values() keeps identity values", {
  res <-
    prepare_human_proxy_values(
      data_source = tibble::tibble(
        dataset_id = "a",
        support = "cell",
        radius_km = NA_real_,
        age_bp = 500,
        value = 0.25
      ),
      proxy = "kk10",
      native_ages_bp = base::seq(8000, 100)
    )

  testthat::expect_equal(dplyr::pull(res, "value_transformed"), 0.25)
  testthat::expect_true(dplyr::pull(res, "is_native_layer"))
  testthat::expect_type(dplyr::pull(res, "is_native_layer"), "logical")
})

testthat::test_that("prepare_human_proxy_values() validates inputs", {
  data_source <-
    tibble::tibble(
      dataset_id = "a",
      support = "cell",
      radius_km = NA_real_,
      age_bp = 500,
      value = -1
    )

  testthat::expect_error(
    prepare_human_proxy_values(
      data_source = data_source,
      proxy = "spd",
      native_ages_bp = 500,
      transformation = "square_root"
    ),
    regexp = "negative"
  )
  testthat::expect_error(
    prepare_human_proxy_values(
      data_source = dplyr::select(data_source, -"support"),
      proxy = "spd",
      native_ages_bp = 500
    ),
    regexp = "contract"
  )
  testthat::expect_error(
    prepare_human_proxy_values(
      data_source = data_source,
      proxy = "spd",
      native_ages_bp = 500,
      transformation = "log"
    ),
    regexp = "contract"
  )
  testthat::expect_error(
    prepare_human_proxy_values(
      data_source = data_source,
      proxy = c("a", "b"),
      native_ages_bp = 500
    ),
    regexp = "contract"
  )
})

testthat::test_that("prepare_human_proxy_values() accepts zero rows", {
  res <-
    prepare_human_proxy_values(
      data_source = tibble::tibble(
        dataset_id = character(),
        support = character(),
        radius_km = numeric(),
        age_bp = numeric(),
        value = numeric()
      ),
      proxy = "kk10",
      native_ages_bp = 500
    )

  testthat::expect_equal(base::nrow(res), 0L)
  testthat::expect_equal(base::ncol(res), 8L)
})
