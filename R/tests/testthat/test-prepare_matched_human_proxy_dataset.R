testthat::test_that("prepare_matched_human_proxy_dataset() joins exact ages", {
  data <- tibble::tibble(age = c(1500, 2000, 2500), spd = c(1, 2, 3))
  proxies <- tibble::tibble(
    dataset_id = "a", age = c(2000, 2500), spd_raw = c(2, 3),
    spd_sqrt = sqrt(c(2, 3))
  )
  result <- prepare_matched_human_proxy_dataset(data, "a", proxies)
  testthat::expect_equal(dplyr::pull(result, age), c(2000, 2500))
  testthat::expect_error(
    prepare_matched_human_proxy_dataset(
      data, "a", dplyr::mutate(proxies, spd_raw = 9)
    ),
    "does not agree"
  )
})
