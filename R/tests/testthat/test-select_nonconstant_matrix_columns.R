testthat::test_that(
  "select_nonconstant_matrix_columns() retains variable columns",
  {
    mat_input <-
      cbind(
        constant = rep(1, 4),
        variable = 1:4
      )

    mat_result <-
      select_nonconstant_matrix_columns(data_matrix = mat_input)

    testthat::expect_true(is.matrix(mat_result))
    testthat::expect_identical(colnames(mat_result), "variable")
    testthat::expect_equal(as.numeric(mat_result), 1:4)
  }
)

testthat::test_that(
  "select_nonconstant_matrix_columns() handles all-constant input",
  {
    mat_result <-
      select_nonconstant_matrix_columns(
        data_matrix = matrix(1, nrow = 3, ncol = 2)
      )

    testthat::expect_equal(dim(mat_result), c(3L, 0L))
  }
)

testthat::test_that(
  "select_nonconstant_matrix_columns() validates its input",
  {
    testthat::expect_error(
      select_nonconstant_matrix_columns(data_matrix = 1:3),
      "matrix or data frame"
    )
  }
)
