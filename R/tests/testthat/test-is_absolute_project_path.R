testthat::test_that("absolute project paths are detected", {
  testthat::expect_true(
    is_absolute_project_path("C:/project/file.R")
  )

  testthat::expect_true(
    is_absolute_project_path("/project/file.R")
  )

  testthat::expect_true(
    is_absolute_project_path("\\\\server\\project\\file.R")
  )

  testthat::expect_false(
    is_absolute_project_path("R/tests/testthat")
  )
})

testthat::test_that("absolute project paths validate input", {
  testthat::expect_error(
    is_absolute_project_path(character()),
    "one non-missing character path"
  )
})
