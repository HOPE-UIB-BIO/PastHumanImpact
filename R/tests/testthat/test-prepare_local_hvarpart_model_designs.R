testthat::test_that("prepare_local_hvarpart_model_designs() reuses climate", {
  set.seed(900723)
  nested <- tibble::tibble(
    dataset_id = "one",
    data_merge = list(tibble::tibble(
      spd_sqrt = sqrt(seq_len(20)),
      kk10_fraction = stats::runif(20),
      hyde_sqrt = stats::runif(20),
      temp_annual = stats::rnorm(20),
      temp_cold = stats::rnorm(20),
      prec_summer = stats::rnorm(20),
      prec_win = stats::rnorm(20)
    ))
  )
  result <- prepare_local_hvarpart_model_designs(
    nested, build_human_proxy_model_specifications(), "dataset_id"
  )
  testthat::expect_equal(nrow(result), 2L)
  climate <- purrr::map(result[["predictor_vars"]], "climate")
  testthat::expect_identical(climate[[1]], climate[[2]])
  testthat::expect_true(all(is.na(result[["selection_error"]])))
  testthat::expect_error(
    prepare_local_hvarpart_model_designs(data.frame(x = 1), data.frame(), "x"),
    "contract"
  )
})
