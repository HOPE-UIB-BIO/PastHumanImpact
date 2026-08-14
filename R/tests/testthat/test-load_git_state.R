testthat::test_that("load_git_state() reports clean and dirty repositories", {
  testthat::skip_if(Sys.which("git") == "", "Git is not installed")

  repo_path <-
    tempfile(pattern = "git-state-")
  dir.create(repo_path)

  system2(
    command = "git",
    args = c("-C", shQuote(repo_path), "init", "--quiet")
  )
  system2(
    command = "git",
    args = c(
      "-C",
      shQuote(repo_path),
      "config",
      "user.email",
      "test@example.com"
    )
  )
  system2(
    command = "git",
    args = c(
      "-C",
      shQuote(repo_path),
      "config",
      "user.name",
      "Test User"
    )
  )

  tracked_path <-
    file.path(repo_path, "tracked.txt")
  writeLines("initial", tracked_path)
  system2(
    command = "git",
    args = c("-C", shQuote(repo_path), "add", "tracked.txt")
  )
  system2(
    command = "git",
    args = c("-C", shQuote(repo_path), "commit", "--quiet", "-m", "initial")
  )

  clean_state <-
    load_git_state(
      repo_path = repo_path,
      verbose = FALSE
    )

  testthat::expect_s3_class(clean_state, "tbl_df")
  testthat::expect_named(clean_state, c("git_commit", "git_is_dirty"))
  testthat::expect_match(clean_state[["git_commit"]], "^[[:xdigit:]]+$")
  testthat::expect_false(clean_state[["git_is_dirty"]])

  writeLines(c("initial", "changed"), tracked_path)

  dirty_state <-
    load_git_state(
      repo_path = repo_path,
      verbose = FALSE
    )

  testthat::expect_identical(
    dirty_state[["git_commit"]],
    clean_state[["git_commit"]]
  )
  testthat::expect_true(dirty_state[["git_is_dirty"]])
})

testthat::test_that("load_git_state() returns missing values after Git failure", {
  result <-
    load_git_state(
      repo_path = tempdir(),
      git_command = tempfile(pattern = "missing-git-"),
      verbose = FALSE
    )

  testthat::expect_true(is.na(result[["git_commit"]]))
  testthat::expect_true(is.na(result[["git_is_dirty"]]))
})

testthat::test_that("load_git_state() validates the repository path", {
  testthat::expect_error(
    load_git_state(
      repo_path = tempfile(pattern = "missing-repo-")
    ),
    regexp = "existing directory"
  )
})
