#' @title Validate project function names
#' @description
#' Enforce the approved function verbs and function-to-file basename contract.
#' @param function_dir Directory containing project function files.
#' @param approved_verbs Character vector of approved function-name verbs.
#' @return Invisibly returns one validation row per function file.
validate_function_names <- function(
  function_dir = here::here("R", "functions"),
  approved_verbs = c(
    "load",
    "save",
    "build",
    "prepare",
    "validate",
    "diagnose",
    "is",
    "has",
    "resolve",
    "compute",
    "aggregate",
    "summarise",
    "fit",
    "predict",
    "score",
    "evaluate",
    "select",
    "run",
    "plot",
    "render",
    "filter",
    "classify",
    "interpolate",
    "scale",
    "project",
    "cluster",
    "deduplicate",
    "normalise",
    "add",
    "cast",
    "initialize",
    "reconcile"
  )
) {
  assertthat::assert_that(
    is.character(function_dir),
    length(function_dir) == 1L,
    !is.na(function_dir),
    dir.exists(function_dir),
    is.character(approved_verbs),
    length(approved_verbs) > 0L,
    !anyNA(approved_verbs),
    msg = "Function-name validation inputs do not satisfy the contract."
  )

  function_files <-
    list.files(
      path = function_dir,
      pattern = "[.]R$",
      recursive = TRUE,
      full.names = TRUE
    )

  validation <-
    function_files |>
    purrr::map_dfr(
      .f = ~ {
        function_file <- .x

        file_lines <-
          readLines(
            con = function_file,
            warn = FALSE,
            encoding = "UTF-8"
          )

        declaration <-
          stringr::str_match(
            file_lines,
            "^([.]?[a-z][a-z0-9_]*)[[:space:]]*<-[[:space:]]*function"
          )[, 2]

        function_name <-
          stats::na.omit(declaration)

        public_name <-
          stringr::str_remove(function_name, "^[.]")

        verb <-
          stringr::str_extract(public_name, "^[^_]+")

        tibble::tibble(
          function_file = function_file,
          function_name = function_name,
          has_one_function = length(function_name) == 1L,
          uses_lower_snake_case = stringr::str_detect(
            public_name,
            "^[a-z][a-z0-9]*(?:_[a-z0-9]+)+$"
          ),
          uses_approved_verb = verb %in% approved_verbs,
          basename_matches = basename(function_file) ==
            paste0(public_name, ".R")
        )
      }
    )

  invalid <-
    validation |>
    dplyr::filter(
      !.data[["has_one_function"]] |
        !.data[["uses_lower_snake_case"]] |
        !.data[["uses_approved_verb"]] |
        !.data[["basename_matches"]]
    )

  if (
    nrow(invalid) > 0L
  ) {
    cli::cli_abort(
      c(
        "Function-name contract failed.",
        "x" = "{nrow(invalid)} function file{?s} violate the contract.",
        "i" = "Inspect the returned audit or run the focused test."
      )
    )
  }

  return(invisible(validation))
}
