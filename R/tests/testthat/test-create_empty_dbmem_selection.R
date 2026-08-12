testthat::test_that(
  "create_empty_dbmem_selection() returns the common schema",
  {
    result_selection <-
      create_empty_dbmem_selection(
        status_value = "rank_deficient",
        n_complete = 12L,
        n_candidates = 3L
      )

    testthat::expect_identical(result_selection[["status"]], "rank_deficient")
    testthat::expect_identical(result_selection[["n_complete"]], 12L)
    testthat::expect_identical(result_selection[["n_candidates"]], 3L)
    testthat::expect_length(result_selection[["selected_names"]], 0L)
    testthat::expect_s3_class(
      result_selection[["selection_table"]],
      "tbl_df"
    )
  }
)

testthat::test_that(
  "create_empty_dbmem_selection() validates counts",
  {
    testthat::expect_error(
      create_empty_dbmem_selection(
        status_value = "invalid",
        n_complete = -1L,
        n_candidates = 0L
      ),
      "contract"
    )
  }
)
