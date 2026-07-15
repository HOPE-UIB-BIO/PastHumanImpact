#' @title Get repository Git state
#' @description
#' Get the current commit and working-tree state for a Git repository. If Git
#' cannot resolve either value, both values are returned as missing.
#' @param repo_path Character scalar path to the Git repository.
#' @param git_command Character scalar Git executable or command name.
#' @param verbose Logical. If `TRUE`, warn when Git state cannot be resolved.
#' @return One-row tibble with `git_commit` and `git_is_dirty` columns.
#' @examples
#' \dontrun{
#' git_state <- get_git_state(repo_path = here::here())
#' }
get_git_state <- function(
  repo_path = ".",
  git_command = "git",
  verbose = TRUE
) {
  assertthat::assert_that(
    is.character(repo_path),
    length(repo_path) == 1L,
    !is.na(repo_path),
    dir.exists(repo_path),
    msg = "`repo_path` must be an existing directory."
  )
  assertthat::assert_that(
    is.character(git_command),
    length(git_command) == 1L,
    !is.na(git_command),
    nzchar(git_command),
    msg = "`git_command` must be a non-empty character scalar."
  )
  assertthat::assert_that(
    is.logical(verbose),
    length(verbose) == 1L,
    !is.na(verbose),
    msg = "`verbose` must be one non-missing logical value."
  )

  repo_path <-
    normalizePath(
      repo_path,
      winslash = "/",
      mustWork = TRUE
    )

  git_commit_result <-
    suppressWarnings(
      tryCatch(
        system2(
          command = git_command,
          args = c(
            "-C",
            shQuote(repo_path),
            "rev-parse",
            "--verify",
            "HEAD"
          ),
          stdout = TRUE,
          stderr = TRUE
        ),
        error = function(err) {
          structure(character(), status = 1L)
        }
      )
    )

  git_status_result <-
    suppressWarnings(
      tryCatch(
        system2(
          command = git_command,
          args = c(
            "-C",
            shQuote(repo_path),
            "status",
            "--porcelain"
          ),
          stdout = TRUE,
          stderr = TRUE
        ),
        error = function(err) {
          structure(character(), status = 1L)
        }
      )
    )

  commit_status <-
    attr(git_commit_result, "status", exact = TRUE)
  working_tree_status <-
    attr(git_status_result, "status", exact = TRUE)
  commit_succeeded <-
    is.null(commit_status) || identical(commit_status, 0L)
  working_tree_succeeded <-
    is.null(working_tree_status) || identical(working_tree_status, 0L)
  git_state_succeeded <-
    commit_succeeded &&
      working_tree_succeeded &&
      length(git_commit_result) == 1L &&
      grepl("^[[:xdigit:]]+$", git_commit_result)

  if (
    isFALSE(git_state_succeeded)
  ) {
    if (
      isTRUE(verbose)
    ) {
      cli::cli_warn(
        "Git state could not be resolved; provenance fields will be missing."
      )
    }

    res_git_state <-
      tibble::tibble(
        git_commit = NA_character_,
        git_is_dirty = NA
      )

    return(res_git_state)
  }

  res_git_state <-
    tibble::tibble(
      git_commit = git_commit_result[1],
      git_is_dirty = length(git_status_result) > 0L
    )

  return(res_git_state)
}
