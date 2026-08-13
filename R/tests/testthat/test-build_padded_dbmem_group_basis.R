testthat::test_that(
  "build_padded_dbmem_group_basis() places and names grouped values",
  {
    mat_result <-
      build_padded_dbmem_group_basis(
        group_basis = matrix(1:4, nrow = 2),
        row_indices = c(1L, 3L),
        n_records = 3L,
        first_mem_index = 4L
      )

    testthat::expect_equal(dim(mat_result), c(3L, 2L))
    testthat::expect_equal(unname(mat_result[2, ]), c(0, 0))
    testthat::expect_identical(
      colnames(mat_result),
      c("dbmem_004", "dbmem_005")
    )
  }
)

testthat::test_that(
  "build_padded_dbmem_group_basis() handles an empty basis",
  {
    mat_result <-
      build_padded_dbmem_group_basis(
        group_basis = matrix(numeric(), nrow = 2, ncol = 0),
        row_indices = c(1L, 2L),
        n_records = 2L,
        first_mem_index = 1L
      )

    testthat::expect_equal(dim(mat_result), c(2L, 0L))
  }
)
