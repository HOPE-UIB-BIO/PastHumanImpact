testthat::test_that("prepared matched inputs preserve mappings and age bounds", {
  within <- tibble::tibble(
    dataset_id = "a",
    data_merge = list(tibble::tibble(
      age = c(1500, 2000, 2500, 8500),
      spd = c(0.1, 0.123, 0.235, 0.4),
      temp_annual = 1:4, temp_cold = 2:5,
      prec_summer = 3:6, prec_win = 4:7
    ))
  )
  slices <- tibble::tibble(
    region = c("Europe", "Europe"), age = c(2000, 2500),
    data_merge = list(
      tibble::tibble(dataset_id = "a", spd = 0.123),
      tibble::tibble(dataset_id = "a", spd = 0.235)
    )
  )
  proxies <- tibble::tibble(
    dataset_id = "a", age_bp = c(1500, 2000, 2500, 8500),
    region = "Europe", spd = c(0.1, 0.1234, 0.2346, 0.4),
    kk10 = c(0.1, 0.2, 0.3, 0.4), hyde = c(1, 4, 9, 16)
  )
  result <- prepare_collinearity_managed_human_proxy_inputs(
    within, slices, proxies,
    tibble::tibble(dataset_id = "a", region = "Europe")
  )
  joined <- result$within_dataset$data_merge[[1]]
  testthat::expect_equal(joined$age, c(2000, 2500))
  testthat::expect_equal(joined$spd_raw, c(0.1234, 0.2346))
  testthat::expect_equal(joined$spd_sqrt, sqrt(joined$spd_raw))
  testthat::expect_equal(joined$kk10_fraction, c(0.2, 0.3))
  testthat::expect_equal(joined$hyde_sqrt, c(2, 3))
})

testthat::test_that("matched input keys and regions are enforced", {
  within <- tibble::tibble(
    dataset_id = "a",
    data_merge = list(tibble::tibble(age = 2000, spd = 1))
  )
  slices <- tibble::tibble(
    region = "Europe", age = 2000,
    data_merge = list(tibble::tibble(dataset_id = "a", spd = 1))
  )
  proxy <- tibble::tibble(
    dataset_id = "a", age_bp = 2000, region = "Asia",
    spd = 1, kk10 = 0.2, hyde = 2
  )
  metadata <- tibble::tibble(dataset_id = "a", region = "Europe")
  testthat::expect_error(
    prepare_collinearity_managed_human_proxy_inputs(
      within, slices, dplyr::bind_rows(proxy, proxy), metadata
    ),
    "contract"
  )
  testthat::expect_error(
    prepare_collinearity_managed_human_proxy_inputs(
      within, slices, proxy, metadata
    ),
    "agree with metadata regions"
  )
})
