testthat::test_that(
  "run_forward_dbmem_selection() returns a selection table",
  {
    set.seed(900723)
    vec_signal <- stats::rnorm(40)
    mat_response <- cbind(response = vec_signal + stats::rnorm(40, sd = 0.1))
    mat_mem <-
      cbind(
        dbmem_001 = vec_signal,
        dbmem_002 = stats::rnorm(40)
      )

    data_result <-
      run_forward_dbmem_selection(
        response_residual = mat_response,
        mem_residual = mat_mem,
        max_selected = 2L,
        adjusted_r_squared = 0.95,
        permutations = 99L,
        alpha = 0.05
      )

    testthat::expect_s3_class(data_result, "tbl_df")
    testthat::expect_s3_class(data_result, "tbl_df")
    testthat::expect_true(
      nrow(data_result) == 0L || "variables" %in% names(data_result)
    )
  }
)

testthat::test_that(
  "run_forward_dbmem_selection() validates row counts",
  {
    testthat::expect_error(
      run_forward_dbmem_selection(
        response_residual = matrix(1:6, nrow = 3),
        mem_residual = matrix(1:8, nrow = 4),
        max_selected = 1L,
        adjusted_r_squared = 0.2,
        permutations = 9L,
        alpha = 0.05
      ),
      "contract"
    )
  }
)
