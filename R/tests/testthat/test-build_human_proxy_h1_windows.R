testthat::test_that("build_human_proxy_h1_windows() builds both windows", {
  data_proxy_windows <-
    tibble::tibble(
      proxy = c("spd", "kk10"),
      age_min_bp = c(2000, 500),
      age_max_bp = c(11500, 8000)
    )

  res <-
    build_human_proxy_h1_windows(
      data_proxy_windows = data_proxy_windows,
      common_window_bp = c(2000, 8000),
      events_window_bp = c(500, 11500)
    )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_named(
    res,
    c("human_proxy", "age_window", "window_id", "age_min_bp", "age_max_bp")
  )
  testthat::expect_equal(base::nrow(res), 6L)

  data_native_kk10 <-
    res |>
    dplyr::filter(
      .data[["human_proxy"]] == "kk10",
      .data[["age_window"]] == "native"
    )

  testthat::expect_equal(
    dplyr::pull(data_native_kk10, "window_id"),
    "native_500_8000"
  )

  vec_common_ids <-
    res |>
    dplyr::filter(.data[["age_window"]] == "common_cohort") |>
    dplyr::pull("window_id") |>
    base::unique()

  testthat::expect_equal(vec_common_ids, "common_cohort_2000_8000")

  data_events <-
    res |>
    dplyr::filter(
      .data[["human_proxy"]] == "events",
      .data[["age_window"]] == "native"
    )

  testthat::expect_equal(dplyr::pull(data_events, "age_min_bp"), 500)
  testthat::expect_equal(dplyr::pull(data_events, "age_max_bp"), 11500)
})

testthat::test_that("build_human_proxy_h1_windows() validates inputs", {
  testthat::expect_error(
    build_human_proxy_h1_windows(
      data_proxy_windows = tibble::tibble(proxy = "spd")
    ),
    regexp = "proxy windows"
  )

  testthat::expect_error(
    build_human_proxy_h1_windows(
      data_proxy_windows = tibble::tibble(
        proxy = "spd",
        age_min_bp = 2000,
        age_max_bp = 8500
      ),
      common_window_bp = c(8000, 2000)
    ),
    regexp = "proxy windows"
  )
})

testthat::test_that("build_human_proxy_h1_windows() needs a shared window", {
  testthat::expect_error(
    build_human_proxy_h1_windows(
      data_proxy_windows = tibble::tibble(
        proxy = "kk10",
        age_min_bp = 500,
        age_max_bp = 6000
      ),
      common_window_bp = c(2000, 8000)
    ),
    regexp = "common window"
  )
})
