testthat::test_that("summarise_human_proxy_coverage() counts records", {
  data_values <-
    tibble::tibble(
      dataset_id = c("a", "b", "c", "d"),
      age_bp = 500,
      proxy = "hyde",
      support = "cell",
      radius_km = NA_real_,
      value = c(0.1, NA, 0.3, 0.4),
      value_transformed = c(0.1, NA, 0.3, 0.4),
      is_native_layer = c(TRUE, FALSE, FALSE, FALSE)
    )

  data_meta <-
    tibble::tibble(
      dataset_id = c("a", "b", "c"),
      region = c("Europe", "Europe", "Asia")
    )

  res <-
    summarise_human_proxy_coverage(
      data_values = data_values,
      data_meta = data_meta
    )

  testthat::expect_named(
    res,
    c(
      "proxy",
      "support",
      "region",
      "age_bp",
      "n_datasets",
      "n_with_value",
      "n_missing",
      "n_native_layer",
      "n_interpolated"
    )
  )

  data_all <-
    res |>
    dplyr::filter(.data[["region"]] == "All regions")

  testthat::expect_equal(dplyr::pull(data_all, "n_datasets"), 4L)
  testthat::expect_equal(dplyr::pull(data_all, "n_with_value"), 3L)
  testthat::expect_equal(dplyr::pull(data_all, "n_missing"), 1L)
  testthat::expect_equal(dplyr::pull(data_all, "n_native_layer"), 1L)
  testthat::expect_equal(dplyr::pull(data_all, "n_interpolated"), 2L)

  data_europe <-
    res |>
    dplyr::filter(.data[["region"]] == "Europe")

  testthat::expect_equal(dplyr::pull(data_europe, "n_datasets"), 2L)
  testthat::expect_equal(dplyr::pull(data_europe, "n_missing"), 1L)

  testthat::expect_true("Unassigned" %in% dplyr::pull(res, "region"))
  testthat::expect_equal(base::nrow(res), 4L)
})

testthat::test_that("summarise_human_proxy_coverage() keeps totals", {
  data_values <-
    tibble::tibble(
      dataset_id = base::rep(c("a", "b"), times = 2),
      age_bp = base::rep(c(500, 1000), each = 2),
      proxy = "kk10",
      support = "spd_buffer",
      radius_km = 250,
      value = c(1, NA, NA, 2),
      value_transformed = c(1, NA, NA, 2),
      is_native_layer = TRUE
    )

  res <-
    summarise_human_proxy_coverage(
      data_values = data_values,
      data_meta = tibble::tibble(
        dataset_id = c("a", "b"),
        region = "Asia"
      )
    )

  testthat::expect_true(
    base::all(
      dplyr::pull(res, "n_with_value") + dplyr::pull(res, "n_missing") ==
        dplyr::pull(res, "n_datasets")
    )
  )
  testthat::expect_true(
    base::all(
      dplyr::pull(res, "n_native_layer") +
        dplyr::pull(res, "n_interpolated") ==
        dplyr::pull(res, "n_with_value")
    )
  )
})

testthat::test_that("summarise_human_proxy_coverage() validates inputs", {
  testthat::expect_error(
    summarise_human_proxy_coverage(
      data_values = tibble::tibble(dataset_id = "a"),
      data_meta = tibble::tibble(dataset_id = "a", region = "Asia")
    ),
    regexp = "contract"
  )
})
