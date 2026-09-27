testthat::test_that("decision memo is evidence-driven and uses public geography terms", {
  memo <- here::here(
    "Manuscript", "COMMSENV-25-2408", "R1", "Internal",
    "human_proxy_hvarpart_decision_memo.qmd"
  )
  testthat::expect_true(file.exists(memo))
  text <- paste(readLines(memo, warn = FALSE), collapse = "\n")
  testthat::expect_match(text, "decision_spatial_summary.csv", fixed = TRUE)
  testthat::expect_match(text, "decision_temporal_summary.csv", fixed = TRUE)
  testthat::expect_match(text, "fig3_canonical_spd_common.png", fixed = TRUE)
  testthat::expect_match(
    text, "fig3_matched_spd_bridge_common.png", fixed = TRUE
  )
  testthat::expect_match(text, "fig3_filtered_joint_common.png", fixed = TRUE)
  testthat::expect_match(
    text, "fig4_three_model_comparison_common.png", fixed = TRUE
  )
  testthat::expect_match(text, "Figure 3 comparison: spatial pattern", fixed = TRUE)
  testthat::expect_match(text, "Figure 4 comparison: temporal pattern", fixed = TRUE)
  testthat::expect_match(
    text,
    "human_proxy_hvarpart_collinearity_managed/00_run.R",
    fixed = TRUE
  )
  testthat::expect_match(text, "`r ", fixed = TRUE)
  testthat::expect_false(grepl(
    "climate[ _-]?zone", text, ignore.case = TRUE, perl = TRUE
  ))
  testthat::expect_false(grepl(
    "(?<!continental )\\bcontinent\\b",
    text, ignore.case = TRUE, perl = TRUE
  ))
})
