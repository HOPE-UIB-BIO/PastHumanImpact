testthat::test_that("display limits retain and count signed tails", {
  data_source <- tibble::tibble(
    signed_allocation = c(-0.2, 0.1, 0.5, 1.2)
  )

  result <- get_hvarpart_display_limits(
    data_source,
    probabilities = c(0.25, 0.75),
    rounding = 0.1
  )

  testthat::expect_equal(result$limits, c(0, 0.7))
  testthat::expect_equal(result$tail_counts$n_below_display, 1)
  testthat::expect_equal(result$tail_counts$n_above_display, 1)
  testthat::expect_equal(result$tail_counts$n_total, 4)
})

testthat::test_that("display limits reject non-finite-only input", {
  testthat::expect_error(
    get_hvarpart_display_limits(
      tibble::tibble(signed_allocation = c(NA_real_, Inf))
    ),
    "No finite"
  )
})
