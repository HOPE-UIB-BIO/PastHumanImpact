testthat::test_that("get_m2_time_df() converts named vector to data frame", {
  vec_m2 <- c(t2 = 0.1, t3 = 0.3)

  res_df <-
    get_m2_time_df(data = vec_m2)

  testthat::expect_s3_class(res_df, "data.frame")
  testthat::expect_equal(names(res_df), c("time", "delta_m2"))
  testthat::expect_equal(dplyr::pull(res_df, time), c("t2", "t3"))
  testthat::expect_equal(dplyr::pull(res_df, delta_m2), c(0.1, 0.3))
})

testthat::test_that("get_m2_time_df() handles unnamed vectors", {
  vec_m2 <- c(0.5, 1.5)

  res_df <-
    get_m2_time_df(data = vec_m2)

  testthat::expect_equal(dplyr::pull(res_df, time), c("1", "2"))
  testthat::expect_equal(dplyr::pull(res_df, delta_m2), vec_m2)
})

testthat::test_that("get_m2_time_df() handles empty vectors", {
  res_df <-
    get_m2_time_df(data = numeric(0))

  testthat::expect_equal(names(res_df), c("time", "delta_m2"))
  testthat::expect_equal(nrow(res_df), 0L)
})

testthat::test_that("get_m2_time_df() validates atomic input", {
  testthat::expect_error(
    get_m2_time_df(data = list(0.1, 0.2)),
    regexp = "atomic vector"
  )
})
