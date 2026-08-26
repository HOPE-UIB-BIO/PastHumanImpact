testthat::test_that("prepare_pollen_percentages() normalises rows", {
  data_counts <-
    data.frame(
      sample_id = c("a", "b"),
      taxon_a = c(1, 3),
      taxon_b = c(1, 1),
      absent = c(0, 0)
    )

  data_result <-
    prepare_pollen_percentages(data_source_counts = data_counts)

  testthat::expect_equal(
    rowSums(data_result[c("taxon_a", "taxon_b")]),
    c(100, 100)
  )

  testthat::expect_false("absent" %in% names(data_result))
})
