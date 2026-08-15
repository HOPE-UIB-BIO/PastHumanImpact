testthat::test_that("compute_chi_square_standardisation() matches formula", {
  mat_source <-
    matrix(c(1, 3, 2, 4), nrow = 2)

  mat_expected <-
    sqrt(sum(mat_source)) * mat_source /
    outer(rowSums(mat_source), sqrt(colSums(mat_source)))

  testthat::expect_equal(
    compute_chi_square_standardisation(mat_source),
    mat_expected
  )
})
