testthat::test_that("normalise_partition_groups() returns consecutive groups", {
  testthat::expect_identical(
    normalise_partition_groups(c(2, 2, 7, 5)),
    c(1L, 1L, 3L, 2L)
  )
})
