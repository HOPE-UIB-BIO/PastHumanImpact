testthat::test_that("make_dir() creates nested directories", {
  target_dir <-
    file.path(
      tempdir(),
      "PastHumanImpact",
      "nested",
      "dir"
    )

  res_make <-
    make_dir(target_dir)

  testthat::expect_true(dir.exists(target_dir))
  testthat::expect_true(is.logical(res_make))
})

testthat::test_that("make_dir() can be called repeatedly", {
  target_dir <-
    file.path(
      tempdir(),
      "PastHumanImpact",
      "existing_dir"
    )

  res_first <-
    make_dir(target_dir)
  res_second <-
    make_dir(target_dir)

  testthat::expect_true(dir.exists(target_dir))
  testthat::expect_true(is.logical(res_first))
  testthat::expect_true(is.logical(res_second))
})

testthat::test_that("make_dir() returns a try-error for invalid path input", {
  res_make <-
    make_dir(dir_path = 1)

  testthat::expect_true(inherits(res_make, "try-error"))
})