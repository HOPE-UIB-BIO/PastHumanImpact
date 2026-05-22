testthat::test_that("extract_m2_time() extracts diagonal -1 values", {
  mat_m2 <-
    matrix(
      c(
        0, NA, NA,
        0.1, 0, NA,
        0.2, 0.3, 0
      ),
      nrow = 3,
      byrow = TRUE,
      dimnames = list(
        c("t1", "t2", "t3"),
        c("t1", "t2", "t3")
      )
    )

  res_vec <-
    extract_m2_time(data = mat_m2)

  testthat::expect_equal(unname(res_vec), c(0.1, 0.3))
  testthat::expect_equal(names(res_vec), c("t2", "t3"))
})

testthat::test_that("extract_m2_time() returns empty vector for one-row input", {
  mat_m2 <-
    matrix(
      0,
      nrow = 1,
      ncol = 1,
      dimnames = list("t1", "t1")
    )

  res_vec <-
    extract_m2_time(data = mat_m2)

  testthat::expect_type(res_vec, "double")
  testthat::expect_length(res_vec, 0L)
})

testthat::test_that("extract_m2_time() errors for non-matrix input", {
  testthat::expect_error(
    extract_m2_time(data = c(1, 2, 3)),
    regexp = "must be a matrix"
  )
})

testthat::test_that("extract_m2_time() validates square matrix input", {
  non_square <-
    matrix(
      c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6),
      nrow = 2,
      ncol = 3
    )

  testthat::expect_error(
    extract_m2_time(data = non_square),
    regexp = "square matrix"
  )
})
