testthat::test_that(
  "prepare_human_proxy_first_differences() points presentward",
  {
  matched <-
    tibble::tibble(
      dataset_id = "a",
      age_bp = c(3000, 2500, 2000),
      region = "Europe",
      spd_transformed = c(1, 3, 6),
      kk10_transformed = c(2, 5, 9),
      hyde_transformed = c(4, 8, 13)
    )

  result <-
    prepare_human_proxy_first_differences(matched)

  testthat::expect_identical(result[["age_bp"]], c(2500, 2000))
  testthat::expect_equal(result[["spd_transformed"]], c(2, 3))
  testthat::expect_equal(result[["hyde_transformed"]], c(4, 5))
})
