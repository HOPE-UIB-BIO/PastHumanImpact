testthat::test_that(
  "select_dbmem_predictors() selects a strong spatial predictor",
  {
    set.seed(900723)
    n_rows <- 60L
    mem_one <- scale(seq_len(n_rows))[, 1]
    mem_two <- stats::rnorm(n_rows)
    data_response <-
      cbind(
        response_one = mem_one + stats::rnorm(n_rows, sd = 0.05),
        response_two = mem_one + stats::rnorm(n_rows, sd = 0.05)
      )
    data_mem <-
      cbind(dbmem_001 = mem_one, dbmem_002 = mem_two)

    result <-
      select_dbmem_predictors(
        response = data_response,
        mem_basis = data_mem,
        conditions = matrix(1, nrow = n_rows, ncol = 1),
        permutations = 99L,
        min_residual_df = 10L,
        seed = 900723L
      )

    testthat::expect_equal(result[["status"]], "selected")
    testthat::expect_true("dbmem_001" %in% result[["selected_names"]])
    testthat::expect_lte(result[["global_p_value"]], 0.05)
  }
)

testthat::test_that(
  "select_dbmem_predictors() handles rank and residual-df limits",
  {
    data_response <- matrix(seq_len(12), ncol = 1)
    data_mem <- matrix(rep(1, 12), ncol = 1)

    result_rank <-
      select_dbmem_predictors(
        response = data_response,
        mem_basis = data_mem,
        permutations = 9L
      )
    result_df <-
      select_dbmem_predictors(
        response = data_response,
        mem_basis = cbind(a = seq_len(12), b = seq_len(12)^2),
        conditions = stats::model.matrix(
          ~ factor(rep(seq_len(6), each = 2))
        ),
        permutations = 9L,
        min_residual_df = 7L
      )

    testthat::expect_equal(result_rank[["status"]], "rank_deficient")
    testthat::expect_equal(
      result_df[["status"]],
      "insufficient_residual_df"
    )
  }
)
