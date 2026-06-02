testthat::test_that("get_sample_rowsum_table() returns sample rowsums", {
  data_counts <-
    tibble::tibble(
      sample_id = c("s1", "s2"),
      taxon_a = c(1, 2),
      taxon_b = c(3, 4)
    )

  res_table <-
    get_sample_rowsum_table(
      data_counts = data_counts
    )

  testthat::expect_s3_class(res_table, "tbl_df")
  testthat::expect_named(res_table, c("sample_id", "rowsum"))
  testthat::expect_equal(dplyr::pull(res_table, rowsum), c(4, 6))
})

testthat::test_that("get_sample_rowsum_table() validates required columns", {
  data_counts <-
    tibble::tibble(
      taxon_a = c(1, 2),
      taxon_b = c(3, 4)
    )

  testthat::expect_error(
    get_sample_rowsum_table(
      data_counts = data_counts
    ),
    "must include `sample_id`"
  )
})
