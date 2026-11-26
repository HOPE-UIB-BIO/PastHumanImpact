data_age_variation_values <-
  tibble::tibble(
    dataset_id = base::rep(c("a", "b", "c"), each = 3),
    age_bp = base::rep(c(1000, 1500, 2000), times = 3),
    proxy = "hyde",
    support = "cell",
    radius_km = NA_real_,
    value = c(0.1, 0.1, 0, 0.3, 0.2, 0.2, NA, NA, NA),
    value_transformed = c(0.1, 0.1, 0, 0.3, 0.2, 0.2, NA, NA, NA),
    is_native_layer = FALSE
  )

testthat::test_that("summarise_human_proxy_age_variation() summarises", {
  res <-
    summarise_human_proxy_age_variation(
      data_values = data_age_variation_values
    ) |>
    dplyr::arrange(dplyr::desc(.data[["age_bp"]]))

  testthat::expect_named(
    res,
    c(
      "proxy",
      "support",
      "age_bp",
      "n_datasets",
      "mean",
      "sd",
      "median",
      "lwr_90",
      "upr_90",
      "max",
      "prop_positive",
      "prop_above_threshold",
      "n_with_change",
      "mean_abs_change",
      "prop_changed"
    )
  )
  testthat::expect_equal(dplyr::pull(res, "age_bp"), c(2000, 1500, 1000))
  testthat::expect_equal(dplyr::pull(res, "n_datasets"), c(2L, 2L, 2L))
  testthat::expect_equal(
    dplyr::pull(res, "mean"),
    c(0.1, 0.15, 0.2),
    tolerance = 1e-8
  )
  testthat::expect_equal(
    dplyr::pull(res, "sd")[[1]],
    base::sqrt(0.02),
    tolerance = 1e-8
  )
  testthat::expect_equal(dplyr::pull(res, "max"), c(0.2, 0.2, 0.3))
  testthat::expect_equal(dplyr::pull(res, "prop_positive"), c(0.5, 1, 1))
  testthat::expect_equal(
    dplyr::pull(res, "lwr_90")[[2]],
    0.105,
    tolerance = 1e-8
  )
  testthat::expect_equal(
    dplyr::pull(res, "upr_90")[[2]],
    0.195,
    tolerance = 1e-8
  )
  testthat::expect_equal(dplyr::pull(res, "n_with_change"), c(0L, 2L, 2L))
  testthat::expect_true(is.na(dplyr::pull(res, "mean_abs_change")[[1]]))
  testthat::expect_equal(
    dplyr::pull(res, "mean_abs_change")[2:3],
    c(0.05, 0.05),
    tolerance = 1e-8
  )
  testthat::expect_equal(
    dplyr::pull(res, "prop_changed")[2:3],
    c(0.5, 0.5)
  )
})

testthat::test_that("summarise_human_proxy_age_variation() uses threshold", {
  res <-
    summarise_human_proxy_age_variation(
      data_values = data_age_variation_values,
      value_threshold = 0.15,
      change_tolerance = 0.2
    ) |>
    dplyr::arrange(dplyr::desc(.data[["age_bp"]]))

  testthat::expect_equal(
    dplyr::pull(res, "prop_above_threshold"),
    c(0.5, 0.5, 0.5)
  )
  testthat::expect_equal(dplyr::pull(res, "prop_changed")[2:3], c(0, 0))
})

testthat::test_that("summarise_human_proxy_age_variation() validates", {
  testthat::expect_error(
    summarise_human_proxy_age_variation(
      data_values = tibble::tibble(dataset_id = "a")
    ),
    regexp = "contract"
  )
  testthat::expect_error(
    summarise_human_proxy_age_variation(
      data_values = data_age_variation_values,
      value_threshold = -1
    ),
    regexp = "contract"
  )
})
