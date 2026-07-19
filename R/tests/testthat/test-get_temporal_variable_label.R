testthat::test_that("get_temporal_variable_label maps temporal names", {
  result <-
    get_temporal_variable_label(
      c("spd", "temp_annual", "n0", "density_turnover")
    )

  testthat::expect_identical(
    result,
    c(
      "SPD",
      "Mean annual temperature",
      "Taxonomic richness",
      "Turnover density"
    )
  )
})

testthat::test_that("get_temporal_variable_label preserves unknown names", {
  testthat::expect_identical(
    get_temporal_variable_label(c("n0", "custom", NA_character_)),
    c("Taxonomic richness", "custom", NA_character_)
  )
})

testthat::test_that("get_temporal_variable_label validates input", {
  testthat::expect_error(
    get_temporal_variable_label(1:3),
    regexp = "character vector"
  )
})
