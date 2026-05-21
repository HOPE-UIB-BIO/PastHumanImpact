testthat::test_that("transform_ages computes age columns after join", {
  testthat::skip_if_not_installed("dplyr")

  data_source <-
    data.frame(
      dataset_id = c(1, 1),
      time_id = c(10, 11),
      time_month_value = c(0, 0)
    )

  trans_data <-
    data.frame(
      timeID = c(10, 11),
      startyear = c(900, 1100),
      endyear = c(1000, 1300)
    )

  result <-
    transform_ages(
      data_source = data_source,
      trans_data = trans_data
    )

  vec_mid_age <-
    result[["mid_age"]]

  vec_age_greg <-
    result[["age_greg"]]

  vec_age <-
    result[["age"]]

  testthat::expect_identical(vec_mid_age, c(950, 1200))
  testthat::expect_identical(vec_age_greg, c(1000, 750))
  testthat::expect_identical(vec_age, c(1000, 750))
})

testthat::test_that("transform_ages adds missing time_month_value as zero", {
  testthat::skip_if_not_installed("dplyr")

  data_source <-
    data.frame(
      dataset_id = 1,
      time_id = 10
    )

  trans_data <-
    data.frame(
      timeID = 10,
      startyear = 900,
      endyear = 1000
    )

  result <-
    transform_ages(
      data_source = data_source,
      trans_data = trans_data
    )

  vec_time_month_value <-
    result[["time_month_value"]]

  vec_age <-
    result[["age"]]

  testthat::expect_identical(vec_time_month_value, 0)
  testthat::expect_identical(vec_age, 1000)
})