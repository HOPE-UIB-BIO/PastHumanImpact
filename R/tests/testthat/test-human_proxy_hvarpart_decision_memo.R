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
      "# What changed since May 2026",
      "# Data scope, analytical populations, and the study interval",
      "# Pollen counts and coverage",
      "# Temporal patterns in the ten pollen assemblage properties",
      "# PAP correlation and the reduced response",
      "# What was corrected in HVarPart",
      "# Model fit is separate from predictor allocation",
      "# Canonical H1 spatial result",
      "# Canonical H1 temporal result",
      "# Canonical H2",
      "# Dependence, thinning, and omission checks",
      "# SPD-radius sensitivity",
      "# Expert-event proxy sensitivity",
      "# Consequences for the manuscript narrative",
      "# Final decision: should human impact include SPD, KK10, and HYDE?"
    )

  purrr::walk(
    required_sections,
    .f = ~ testthat::expect_match(text, .x, fixed = TRUE)
  )

  testthat::expect_match(text, "**Canonical R1**", fixed = TRUE)
  testthat::expect_match(text, "**Registered validation**", fixed = TRUE)
  testthat::expect_match(text, "**Exploratory proxy decision**", fixed = TRUE)

  canonical_position <-
    regexpr("# Canonical H1 spatial result", text, fixed = TRUE)[[1]]

  h2_position <-
    regexpr("# Canonical H2", text, fixed = TRUE)[[1]]

  validation_position <-
    regexpr("# Dependence, thinning, and omission checks", text, fixed = TRUE)[[1]]

  decision_position <-
    regexpr("# Final decision:", text, fixed = TRUE)[[1]]

  decision_box_position <-
    regexpr(
      "\\textbf{Decision requested.}",
      text,
      fixed = TRUE
    )[[1]]

  testthat::expect_gt(decision_position, canonical_position)
  testthat::expect_gt(decision_position, h2_position)
  testthat::expect_gt(decision_position, validation_position)
  testthat::expect_gt(decision_box_position, decision_position)
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
