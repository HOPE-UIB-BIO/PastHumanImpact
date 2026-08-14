#' @title Validate target pipeline style
#' @description
#' Check that a target pipeline documents its runner, uses the project header,
#' quotes every target name, and explains the purpose of every target.
#' @param path_pipeline Path to one target pipeline script.
#' @param runner Repository-relative path to the runner for the pipeline.
#' @return Invisibly returns a one-row pipeline style audit.
#' @examples
#' \dontrun{
#' validate_target_pipeline_style(
#'   path_pipeline = "R/analyses/example/pipeline.R",
#'   runner = "R/analyses/example/00_run.R"
#' )
#' }
validate_target_pipeline_style <- function(
  path_pipeline,
  runner
) {
  assertthat::assert_that(
    is.character(path_pipeline),
    length(path_pipeline) == 1L,
    !is.na(path_pipeline),
    file.exists(path_pipeline),
    is.character(runner),
    length(runner) == 1L,
    !is.na(runner),
    nzchar(runner),
    msg = "Pipeline style validation inputs do not satisfy the contract."
  )

  pipeline_lines <-
    readLines(
      con = path_pipeline,
      warn = FALSE,
      encoding = "UTF-8"
    )

  target_lines <-
    which(
      stringr::str_detect(
        pipeline_lines,
        "^[[:space:]]*targets::tar_target\\("
      )
    )

  quoted_names <-
    target_lines |>
    purrr::map_lgl(
      .f = ~ stringr::str_detect(
        pipeline_lines[[.x + 1L]],
        '^[[:space:]]*name = "[a-z][a-z0-9_]*",?[[:space:]]*$'
      )
    )

  has_why_comment <-
    target_lines |>
    purrr::map_lgl(
      .f = ~ {
        preceding_indices <-
          seq.int(
            from = max(1L, .x - 4L),
            to = .x - 1L
          )

        preceding_lines <-
          pipeline_lines[preceding_indices]

        immediate_comment <-
          stringr::str_detect(
            pipeline_lines[[.x - 1L]],
            "^[[:space:]]*#"
          )

        isTRUE(immediate_comment) &&
          any(stringr::str_detect(preceding_lines, "^[[:space:]]*# Why:"))
      }
    )

  expected_runner_comment <-
    stringr::str_glue("#   {runner}")

  audit <-
    tibble::tibble(
      path_pipeline = path_pipeline,
      runner = runner,
      has_project_header = any(
        pipeline_lines == "#                     GlobalHumanImpact"
      ),
      documents_runner =
        any(pipeline_lines == "# Run with:") &&
        any(pipeline_lines == expected_runner_comment),
      warns_source_only_declares = any(
        pipeline_lines == stringr::str_c(
          "# Sourcing this script only declares targets;",
          " ",
          "it does not execute them."
        )
      ),
      has_configuration_section = any(
        pipeline_lines == "# 0. Configure pipeline -----"
      ),
      has_target_section = any(
        pipeline_lines == "# 1. Define targets -----"
      ),
      has_targets = length(target_lines) > 0L,
      target_names_are_quoted = all(quoted_names),
      targets_explain_why = all(has_why_comment)
    )

  valid <-
    audit |>
    dplyr::select(
      -tidyselect::all_of(
        c("path_pipeline", "runner")
      )
    ) |>
    unlist(
      use.names = FALSE
    ) |>
    all()

  if (
    !isTRUE(valid)
  ) {
    cli::cli_abort(
      c(
        "Target pipeline style contract failed.",
        "x" = "Pipeline: {path_pipeline}",
        "i" = stringr::str_c(
          "Use the common header, document the runner, quote target names,",
          " ",
          "and place a '# Why:' comment immediately above every target."
        )
      )
    )
  }

  return(invisible(audit))
}
