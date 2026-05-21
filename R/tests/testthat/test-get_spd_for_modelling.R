testthat::test_that("get_spd_for_modelling() nests rounded long data by variable", {
  data_source_spd <-
    data.frame(
      dataset_id = 1,
      spd = I(list(data.frame(
        age = c(0, 500),
        var_a = c(1.2345, NA),
        var_b = c(2.3456, 3.4567)
      ))),
      stringsAsFactors = FALSE
    )

  result <-
    get_spd_for_modelling(data_source_spd)

  vec_var_name <-
    result[["var_name"]]

  data_var_a <-
    purrr::pluck(result, "data_to_fit", 1)

  data_var_b <-
    purrr::pluck(result, "data_to_fit", 2)

  testthat::expect_identical(vec_var_name, c("var_a", "var_b"))
  testthat::expect_identical(
    as.data.frame(data_var_a),
    data.frame(
      dataset_id = 1,
      age = 0,
      value = 1.234
    )
  )
  testthat::expect_identical(
    as.data.frame(data_var_b),
    data.frame(
      dataset_id = 1,
      age = c(0, 500),
      value = c(2.346, 3.457)
    )
  )
})

testthat::test_that("get_spd_for_modelling() returns expected var names", {
  data_source_spd <-
    data.frame(
      dataset_id = 2,
      spd = I(list(data.frame(
        age = c(0, 500),
        spd_a = c(1.1111, 2.2222),
        spd_b = c(3.3333, 4.4444)
      ))),
      stringsAsFactors = FALSE
    )

  result <- get_spd_for_modelling(data_source_spd)

  testthat::expect_identical(
    sort(dplyr::pull(result, var_name)),
    c("spd_a", "spd_b")
  )
})

testthat::test_that("get_spd_for_modelling() rounds to three decimals", {
  data_source_spd <-
    data.frame(
      dataset_id = 1,
      spd = I(list(data.frame(
        age = c(0, 500),
        var_x = c(0.12344, 0.12345)
      ))),
      stringsAsFactors = FALSE
    )

  result <- get_spd_for_modelling(data_source_spd)
  data_x <- dplyr::pull(result, data_to_fit)[[1]]

  testthat::expect_identical(dplyr::pull(data_x, value), c(0.123, 0.123))
})