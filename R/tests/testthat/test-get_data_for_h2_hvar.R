testthat::test_that("get_data_for_h2_hvar merges and filters to m2 ages", {
  data_predictors <-
    data.frame(
      region = rep("Europe", 10),
      climatezone = rep("Temperate", 10),
      variable = c(
        "temp_annual", "temp_annual",
        "temp_cold", "temp_cold",
        "prec_summer", "prec_summer",
        "prec_win", "prec_win",
        "spd", "spd"
      ),
      age = c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2),
      value = c(10, 20, 3, 4, 100, 200, 50, 60, 5, 6),
      stringsAsFactors = FALSE
    )

  data_m2 <-
    data.frame(
      region = "Europe",
      climatezone = "Temperate",
      m2 = I(list(matrix(
        c(1, 2, 3, 4),
        nrow = 2,
        dimnames = list(c("a", "b"), c("1", "2"))
      ))),
      stringsAsFactors = FALSE
    )

  result <-
    get_data_for_h2_hvar(
      data_predictors = data_predictors,
      data_m2 = data_m2
    )

  expected <-
    data.frame(
      age = c(1, 2),
      temp_annual = c(10, 20),
      temp_cold = c(3, 4),
      prec_summer = c(100, 200),
      prec_win = c(50, 60),
      spd = c(5, 6)
    )

  vec_region <-
    result[["region"]]

  vec_climatezone <-
    result[["climatezone"]]

  data_data_merge <-
    purrr::pluck(result, "data_merge", 1)

  mat_m2 <-
    purrr::pluck(result, "m2", 1)

  vec_m2_colnames <-
    colnames(mat_m2)

  testthat::expect_identical(vec_region, "Europe")
  testthat::expect_identical(vec_climatezone, "Temperate")
  testthat::expect_identical(data_data_merge, expected)
  testthat::expect_identical(vec_m2_colnames, c("1", "2"))
})

testthat::test_that("get_data_for_h2_hvar() returns empty data when a predictor is missing", {
  data_predictors <-
    data.frame(
      region = rep("Europe", 8),
      climatezone = rep("Temperate", 8),
      variable = c(
        "temp_annual", "temp_annual",
        "temp_cold", "temp_cold",
        "prec_summer", "prec_summer",
        "prec_win", "prec_win"
      ),
      age = c(1, 2, 1, 2, 1, 2, 1, 2),
      value = c(10, 20, 3, 4, 100, 200, 50, 60),
      stringsAsFactors = FALSE
    )

  data_m2 <-
    data.frame(
      region = "Europe",
      climatezone = "Temperate",
      m2 = I(list(matrix(c(1, 2, 3, 4), nrow = 2, dimnames = list(c("a", "b"), c("1", "2"))))),
      stringsAsFactors = FALSE
    )

  result <-
    get_data_for_h2_hvar(
      data_predictors = data_predictors,
      data_m2 = data_m2
    )

  data_data_merge <-
    dplyr::pull(result, data_merge)[[1]]

  testthat::expect_equal(nrow(data_data_merge), 0L)
})

testthat::test_that("get_data_for_h2_hvar() keeps only ages present in m2 colnames", {
  data_predictors <-
    data.frame(
      region = rep("Europe", 15),
      climatezone = rep("Temperate", 15),
      variable = rep(c("temp_annual", "temp_cold", "prec_summer", "prec_win", "spd"), each = 3),
      age = rep(c(1, 2, 3), times = 5),
      value = c(10, 20, 30, 3, 4, 5, 100, 200, 300, 50, 60, 70, 5, 6, 7),
      stringsAsFactors = FALSE
    )

  data_m2 <-
    data.frame(
      region = "Europe",
      climatezone = "Temperate",
      m2 = I(list(matrix(c(1, 2, 3, 4), nrow = 2, dimnames = list(c("a", "b"), c("2", "3"))))),
      stringsAsFactors = FALSE
    )

  result <-
    get_data_for_h2_hvar(
      data_predictors = data_predictors,
      data_m2 = data_m2
    )

  data_data_merge <-
    dplyr::pull(result, data_merge)[[1]]

  testthat::expect_identical(dplyr::pull(data_data_merge, age), c(2, 3))
})

testthat::test_that("get_data_for_h2_hvar() validates predictor columns", {
  data_predictors <-
    data.frame(
      region = "Europe",
      climatezone = "Temperate",
      variable = "temp_annual",
      age = 1,
      stringsAsFactors = FALSE
    )

  data_m2 <-
    data.frame(
      region = "Europe",
      climatezone = "Temperate",
      m2 = I(list(matrix(1, nrow = 1, ncol = 1, dimnames = list("a", "1")))),
      stringsAsFactors = FALSE
    )

  testthat::expect_error(
    get_data_for_h2_hvar(
      data_predictors = data_predictors,
      data_m2 = data_m2
    ),
    regexp = "data_predictors"
  )
})