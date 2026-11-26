testthat::test_that("build_human_proxy_h1_specifications() maps profiles", {
  data_profiles <-
    tibble::tibble(
      profile_id = c("kk10_native", "events_common"),
      analytical_unit = c("within_dataset", "time_slice"),
      human_proxy = c("kk10", "events"),
      proxy_support = c("radius_50km", "not_applicable"),
      age_window = c("native", "common_cohort")
    )

  data_windows <-
    build_human_proxy_h1_windows(
      data_proxy_windows = tibble::tibble(
        proxy = "kk10",
        age_min_bp = 500,
        age_max_bp = 8000
      )
    )

  res <-
    build_human_proxy_h1_specifications(
      data_profiles = data_profiles,
      data_windows = data_windows,
      event_columns = c("fi", "fc")
    )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_equal(base::nrow(res), 2L)
  testthat::expect_true(
    base::all(
      c(
        "model_id", "model_label", "human_candidates", "human_preference",
        "exploratory", "analytical_unit", "human_proxy", "proxy_support",
        "age_window", "window_id", "age_min_bp", "age_max_bp"
      ) %in% base::names(res)
    )
  )
  testthat::expect_equal(
    dplyr::pull(res, "model_id"),
    c("kk10_native", "events_common")
  )
  testthat::expect_equal(
    dplyr::pull(res, "human_candidates"),
    list("kk10__radius_50km", c("fi", "fc"))
  )
  testthat::expect_equal(
    dplyr::pull(res, "window_id"),
    c("native_500_8000", "common_cohort_2000_8000")
  )
  testthat::expect_false(base::any(dplyr::pull(res, "exploratory")))
})

testthat::test_that("build_human_proxy_h1_specifications() checks inputs", {
  testthat::expect_error(
    build_human_proxy_h1_specifications(
      data_profiles = tibble::tibble(profile_id = "x"),
      data_windows = tibble::tibble(),
      event_columns = "fi"
    ),
    regexp = "specification inputs"
  )
})

testthat::test_that("build_human_proxy_h1_specifications() needs a window", {
  data_profiles <-
    tibble::tibble(
      profile_id = "hyde_native",
      analytical_unit = "within_dataset",
      human_proxy = "hyde_3_5_land_use",
      proxy_support = "spd_buffer",
      age_window = "native"
    )

  data_windows <-
    build_human_proxy_h1_windows(
      data_proxy_windows = tibble::tibble(
        proxy = "kk10",
        age_min_bp = 500,
        age_max_bp = 8000
      )
    )

  testthat::expect_error(
    build_human_proxy_h1_specifications(
      data_profiles = data_profiles,
      data_windows = data_windows,
      event_columns = "fi"
    ),
    regexp = "no age window"
  )
})
