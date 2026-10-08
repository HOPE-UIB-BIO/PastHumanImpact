testthat::test_that("co-author memo covers the full revision before the proxy decision", {
  memo <-
    here::here(
      "Manuscript", "COMMSENV-25-2408", "R1", "Internal",
      "human_proxy_hvarpart_decision_memo.qmd"
    )

  testthat::expect_true(file.exists(memo))

  text <-
    paste(readLines(memo, warn = FALSE), collapse = "\n")

  required_sections <-
    c(
      "# How the revision developed",
      "# First, clarifying which data enter each analysis",
      "# Checking pollen counts and the cost of stricter thresholds",
      "# Refitting the pollen assemblage trajectories",
      "# Checking whether overlapping PAPs drive the result",
      "# Correcting how I extract and interpret HVarPart results",
      "# Showing model fit alongside predictor allocation",
      "# Adding time control to spatial H1",
      "# Adding spatial control to temporal H1",
      "# Revisiting what H2 tells us",
      "# Checking dependence and uneven sampling",
      "# Testing the archaeological neighbourhood radius",
      "# Comparing SPD with the expert-event histories",
      "# Bringing these changes back into the manuscript",
      "# Final decision: should human impact include SPD, KK10, and HYDE?"
    )

  purrr::walk(
    required_sections,
    .f = ~ testthat::expect_match(text, .x, fixed = TRUE)
  )

  testthat::expect_match(
    text,
    "**The revised manuscript analyses:**",
    fixed = TRUE
  )
  testthat::expect_match(text, "**The supporting checks:**", fixed = TRUE)
  testthat::expect_match(text, "**The later exploratory work:**", fixed = TRUE)

  canonical_position <-
    regexpr("# Adding time control to spatial H1", text, fixed = TRUE)[[1]]

  h2_position <-
    regexpr("# Revisiting what H2 tells us", text, fixed = TRUE)[[1]]

  validation_position <-
    regexpr("# Checking dependence and uneven sampling", text, fixed = TRUE)[[1]]

  decision_position <-
    regexpr("# Final decision:", text, fixed = TRUE)[[1]]

  decision_input_position <-
    regexpr(
      "## Where I need your input",
      text,
      fixed = TRUE
    )[[1]]

  testthat::expect_gt(decision_position, canonical_position)
  testthat::expect_gt(decision_position, h2_position)
  testthat::expect_gt(decision_position, validation_position)
  testthat::expect_gt(decision_input_position, decision_position)
})

testthat::test_that("co-author memo reads public evidence and fails actionably", {
  memo <-
    here::here(
      "Manuscript", "COMMSENV-25-2408", "R1", "Internal",
      "human_proxy_hvarpart_decision_memo.qmd"
    )

  text <-
    paste(readLines(memo, warn = FALSE), collapse = "\n")

  expected_evidence <-
    c(
      "../_sections/00-evidence-setup.qmd",
      "revision_h1_spatial_figure",
      "revision_h1_temporal_figure",
      "revision_h2_figure",
      "decision_spatial_summary.csv",
      "decision_temporal_summary.csv",
      "fig3_matched_spd_bridge_common.png",
      "fig3_filtered_joint_common.png",
      "fig4_three_model_comparison_common.png",
      "shared_climate_selection.csv"
    )

  purrr::walk(
    expected_evidence,
    .f = ~ testthat::expect_match(text, .x, fixed = TRUE)
  )

  testthat::expect_match(
    text,
    "Rscript R/analyses/06_reporting/00_run.R",
    fixed = TRUE
  )
  testthat::expect_match(
    text,
    "scripts/assemble_revision_assets.R",
    fixed = TRUE
  )
  testthat::expect_match(
    text,
    "human_proxy_hvarpart_collinearity_managed/00_run.R",
    fixed = TRUE
  )
  testthat::expect_match(text, "`r ", fixed = TRUE)
  testthat::expect_match(text, "identical_climate_set", fixed = TRUE)
  testthat::expect_match(text, "stack_check", fixed = TRUE)
  testthat::expect_match(text, "model_error", fixed = TRUE)
})

testthat::test_that("co-author memo uses public geography terminology", {
  memo <-
    here::here(
      "Manuscript", "COMMSENV-25-2408", "R1", "Internal",
      "human_proxy_hvarpart_decision_memo.qmd"
    )

  text <-
    paste(readLines(memo, warn = FALSE), collapse = "\n")

  public_text <-
    gsub(
      "```\\{r\\}[\\s\\S]*?```",
      "",
      text,
      perl = TRUE
    )

  public_text <-
    gsub("`[^`]*`", "", public_text, perl = TRUE)

  testthat::expect_false(
    grepl(
      "climate[ _-]?zone",
      public_text,
      ignore.case = TRUE,
      perl = TRUE
    )
  )
  testthat::expect_false(
    grepl(
      "(?<!continental[- ]region)\\bcontinent\\b",
      public_text,
      ignore.case = TRUE,
      perl = TRUE
    )
  )
})
