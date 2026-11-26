testthat::test_that("classify_human_climate_outcome() reads intervals", {
  res <-
    classify_human_climate_outcome(
      lwr = c(0.1, -0.5, -0.2, 0, NA, -0.4),
      upr = c(0.3, -0.1, 0.2, 0.3, 0.2, 0)
    )

  testthat::expect_type(res, "character")
  testthat::expect_equal(
    res,
    c(
      "human", "climate", "no_clear_dominance", "no_clear_dominance", NA,
      "no_clear_dominance"
    )
  )
})

testthat::test_that("classify_human_climate_outcome() validates inputs", {
  testthat::expect_error(
    classify_human_climate_outcome(lwr = c(0.1, 0.2), upr = 0.3),
    regexp = "Interval bounds"
  )

  testthat::expect_error(
    classify_human_climate_outcome(lwr = "a", upr = "b"),
    regexp = "Interval bounds"
  )
})

testthat::test_that("classify_human_climate_outcome() handles no input", {
  res <-
    classify_human_climate_outcome(lwr = numeric(), upr = numeric())

  testthat::expect_identical(res, character())
})
