testthat::test_that("run_pcoa() returns points and eigenvalues", {
  mat_m2 <-
    matrix(
      c(
        0, 0.5, 0.8,
        0.5, 0, 0.3,
        0.8, 0.3, 0
      ),
      nrow = 3,
      dimnames = list(c("a", "b", "c"), c("a", "b", "c"))
    )

  res_pcoa <-
    run_pcoa(data_m2 = mat_m2)

  testthat::expect_type(res_pcoa, "list")
  testthat::expect_true("points" %in% names(res_pcoa))
  testthat::expect_true("eig" %in% names(res_pcoa))
  testthat::expect_equal(nrow(res_pcoa$points), 3L)
  testthat::expect_equal(length(res_pcoa$eig), 3L)
})

testthat::test_that("run_pcoa() accepts non-symmetric matrices", {
  mat_m2 <-
    matrix(
      c(
        0, 0.2, 0.7,
        0.5, 0, 0.3,
        0.8, 0.1, 0
      ),
      nrow = 3
    )

  res_pcoa <- NULL
  testthat::expect_warning(
    res_pcoa <- run_pcoa(data_m2 = mat_m2),
    regexp = "only 1 of the first 2 eigenvalues are > 0"
  )

  testthat::expect_equal(nrow(res_pcoa$points), 3L)
})

testthat::test_that("run_pcoa() errors when distances include NA", {
  mat_m2 <-
    matrix(
      c(
        0, 0.5, NA,
        0.5, 0, 0.3,
        NA, 0.3, 0
      ),
      nrow = 3
    )

  testthat::expect_error(
    run_pcoa(data_m2 = mat_m2),
    regexp = "must not contain NA"
  )
})

testthat::test_that("run_pcoa() validates matrix input", {
  testthat::expect_error(
    run_pcoa(data_m2 = c(0, 1, 2)),
    regexp = "must be a matrix"
  )
})
