testthat::test_that("prepare_human_proxy_matches() joins and transforms", {
  spd <-
    tibble::tibble(
      dataset_id = "a",
      age_bp = 2000,
      spd = 4,
      radius_km = 250
    )

  kk10 <-
    tibble::tibble(dataset_id = "a", age_bp = 2000, value = 0.25)

  hyde <-
    tibble::tibble(dataset_id = "a", age_bp = 2000, value = 9)

  metadata <-
    tibble::tibble(dataset_id = "a", region = "Europe")

  result <-
    prepare_human_proxy_matches(spd, kk10, hyde, metadata)

  testthat::expect_equal(result[["spd_transformed"]], 2)
  testthat::expect_equal(result[["kk10_transformed"]], 0.25)
  testthat::expect_equal(result[["hyde_transformed"]], 3)
})
