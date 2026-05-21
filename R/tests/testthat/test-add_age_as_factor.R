testthat::test_that("add_age_as_factor() converts years to kyr factor", {
  data_input <-
    data.frame(
      age = c(0, 500, 1000, 8500),
      value = c("a", "b", "c", "d")
    )

  res_data <-
    add_age_as_factor(data_source = data_input)

  vec_age <-
    dplyr::pull(res_data, age)

  testthat::expect_s3_class(vec_age, "factor")
  testthat::expect_identical(
    as.character(vec_age),
    c("0", "0.5", "1", "8.5")
  )
})

testthat::test_that("add_age_as_factor() preserves non-age columns", {
  data_input <-
    data.frame(
      age = c(0, 500, 1000),
      value = c("x", "y", "z"),
      score = c(1.1, 2.2, 3.3)
    )

  res_data <-
    add_age_as_factor(data_source = data_input)

  testthat::expect_identical(
    dplyr::pull(res_data, value),
    dplyr::pull(data_input, value)
  )
  testthat::expect_identical(
    dplyr::pull(res_data, score),
    dplyr::pull(data_input, score)
  )
  testthat::expect_equal(nrow(res_data), 3L)
})

testthat::test_that("add_age_as_factor() sets out-of-range ages to NA", {
  data_input <-
    data.frame(
      age = c(9000, NA_real_, 2000)
    )

  res_data <-
    add_age_as_factor(data_source = data_input)

  vec_age <-
    dplyr::pull(res_data, age)

  testthat::expect_true(is.na(vec_age[[1]]))
  testthat::expect_true(is.na(vec_age[[2]]))
  testthat::expect_identical(as.character(vec_age[[3]]), "2")
})