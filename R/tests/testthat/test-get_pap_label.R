testthat::test_that("get_pap_label maps PAP names", {
  result <-
    get_pap_label(
      c("n0", "n1_divided_by_n0", "roc", "density_turnover")
    )

  testthat::expect_identical(
    result,
    c(
      "Taxonomic richness",
      "Shannon diversity / richness",
      "Rate of change",
      "Turnover density"
    )
  )
})

testthat::test_that("get_pap_label preserves unknown names", {
  testthat::expect_identical(
    get_pap_label(c("n0", "custom_pap", NA_character_)),
    c("Taxonomic richness", "custom_pap", NA_character_)
  )
})

testthat::test_that("get_pap_label validates input", {
  testthat::expect_error(
    get_pap_label(1:3),
    regexp = "character vector"
  )
})
