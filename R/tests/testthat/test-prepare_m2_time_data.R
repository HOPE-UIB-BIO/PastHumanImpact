testthat::test_that("prepare_m2_time_data() converts named vector to data frame", {
  vec_m2 <- c(t2 = 0.1, t3 = 0.3)

  res_df <-
    prepare_m2_time_data(data = vec_m2)

  testthat::expect_s3_class(res_df, "data.frame")
  testthat::expect_equal(names(res_df), c("time", "delta_m2"))
  testthat::expect_equal(dplyr::pull(res_df, time), c("t2", "t3"))
  testthat::expect_equal(dplyr::pull(res_df, delta_m2), c(0.1, 0.3))
})

testthat::test_that("prepare_m2_time_data() handles unnamed vectors", {
  vec_m2 <- c(0.5, 1.5)

  res_df <-
    prepare_m2_time_data(data = vec_m2)

  testthat::expect_equal(dplyr::pull(res_df, time), c("1", "2"))
  testthat::expect_equal(dplyr::pull(res_df, delta_m2), vec_m2)
})

testthat::test_that("prepare_m2_time_data() handles empty vectors", {
  res_df <-
    prepare_m2_time_data(data = numeric(0))

  testthat::expect_equal(names(res_df), c("time", "delta_m2"))
  testthat::expect_equal(nrow(res_df), 0L)
})

testthat::test_that("prepare_m2_time_data() validates atomic input", {
  testthat::expect_error(
    prepare_m2_time_data(data = list(0.1, 0.2)),
    regexp = "atomic vector"
  )
})
