testthat::test_that(
  "target pipeline style accepts the documented contract",
  {
    path_pipeline <- tempfile(fileext = ".R")

    pipeline_lines <-
      c(
        "#                     GlobalHumanImpact",
        "# Run with:",
        "#   R/analyses/example/00_run.R",
        stringr::str_c(
          "# Sourcing this script only declares targets;",
          " ",
          "it does not execute them."
        ),
        "# 0. Configure pipeline -----",
        "# 1. Define targets -----",
        "# Why: Preserve one value so downstream work has a stable input.",
        "targets::tar_target(",
        '  name = "result_example",',
        "  command = 1L",
        ")"
      )

    writeLines(pipeline_lines, path_pipeline)

    testthat::expect_silent(
      validate_target_pipeline_style(
        path_pipeline = path_pipeline,
        runner = "R/analyses/example/00_run.R"
      )
    )
  }
)

testthat::test_that(
  "target pipeline style rejects unquoted target names",
  {
    path_pipeline <- tempfile(fileext = ".R")

    pipeline_lines <-
      c(
        "#                     GlobalHumanImpact",
        "# Run with:",
        "#   R/analyses/example/00_run.R",
        stringr::str_c(
          "# Sourcing this script only declares targets;",
          " ",
          "it does not execute them."
        ),
        "# 0. Configure pipeline -----",
        "# 1. Define targets -----",
        "# Why: Preserve one value so downstream work has a stable input.",
        "targets::tar_target(",
        "  name = result_example,",
        "  command = 1L",
        ")"
      )

    writeLines(pipeline_lines, path_pipeline)

    testthat::expect_error(
      validate_target_pipeline_style(
        path_pipeline = path_pipeline,
        runner = "R/analyses/example/00_run.R"
      ),
      regexp = "style contract failed"
    )
  }
)

testthat::test_that(
  "target pipeline style requires a reason for every target",
  {
    path_pipeline <- tempfile(fileext = ".R")

    pipeline_lines <-
      c(
        "#                     GlobalHumanImpact",
        "# Run with:",
        "#   R/analyses/example/00_run.R",
        stringr::str_c(
          "# Sourcing this script only declares targets;",
          " ",
          "it does not execute them."
        ),
        "# 0. Configure pipeline -----",
        "# 1. Define targets -----",
        "targets::tar_target(",
        '  name = "result_example",',
        "  command = 1L",
        ")"
      )

    writeLines(pipeline_lines, path_pipeline)

    testthat::expect_error(
      validate_target_pipeline_style(
        path_pipeline = path_pipeline,
        runner = "R/analyses/example/00_run.R"
      ),
      regexp = "style contract failed"
    )
  }
)

testthat::test_that(
  "all registered target pipelines follow the style contract",
  {
    path_contracts <-
      here::here(
        "R",
        "analyses",
        "00_profiles",
        "pipeline_contracts.csv"
      )

    data_contracts <-
      readr::read_csv(
        file = path_contracts,
        show_col_types = FALSE
      )

    testthat::expect_silent(
      purrr::pwalk(
        .l = list(
          path_pipeline = here::here(data_contracts[["script"]]),
          runner = data_contracts[["runner"]]
        ),
        .f = validate_target_pipeline_style
      )
    )
  }
)
