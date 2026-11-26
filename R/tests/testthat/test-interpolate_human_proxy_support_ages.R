data_interpolation_extracted <-
  tibble::tibble(
    dataset_id = c("a", "a", "a", "a", "b", "b"),
    support = c("cell", "cell", "spd_buffer", "spd_buffer", "cell", "cell"),
    radius_km = c(NA, NA, 250, 250, NA, NA),
    age_bp = c(1950, 950, 1950, 950, 1950, 950),
    value = c(0, 1, 10, 20, NA, NA)
  )

testthat::test_that("interpolate_human_proxy_support_ages() interpolates", {
  res <-
    interpolate_human_proxy_support_ages(
      data_extracted = data_interpolation_extracted,
      target_ages_bp = c(1500, 1950)
    ) |>
    dplyr::arrange(
      .data[["dataset_id"]],
      .data[["support"]],
      .data[["age_bp"]]
    )

  testthat::expect_named(
    res,
    c(
      "dataset_id",
      "support",
      "radius_km",
      "age_bp",
      "value",
      "source_age_younger",
      "source_age_older",
      "interpolation_weight"
    )
  )
  testthat::expect_equal(base::nrow(res), 6L)
  testthat::expect_equal(
    dplyr::pull(res, "value"),
    c(0.45, 0, 14.5, 10, NA, NA),
    tolerance = 1e-8
  )
  testthat::expect_equal(
    dplyr::pull(res, "radius_km"),
    c(NA, NA, 250, 250, NA, NA)
  )
  testthat::expect_equal(
    dplyr::pull(res, "interpolation_weight")[1:2],
    c(0.55, 0),
    tolerance = 1e-8
  )
})

testthat::test_that("interpolate_human_proxy_support_ages() never extends", {
  res <-
    interpolate_human_proxy_support_ages(
      data_extracted = data_interpolation_extracted,
      target_ages_bp = c(500, 2500)
    )

  testthat::expect_true(base::all(is.na(dplyr::pull(res, "value"))))
})

testthat::test_that("interpolate_human_proxy_support_ages() validates", {
  testthat::expect_error(
    interpolate_human_proxy_support_ages(
      data_extracted = dplyr::select(data_interpolation_extracted, -"support"),
      target_ages_bp = 1500
    ),
    regexp = "contract"
  )
  testthat::expect_error(
    interpolate_human_proxy_support_ages(
      data_extracted = data_interpolation_extracted,
      target_ages_bp = "1500"
    ),
    regexp = "contract"
  )
})
