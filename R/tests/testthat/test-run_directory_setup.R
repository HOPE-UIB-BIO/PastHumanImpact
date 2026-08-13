testthat::test_that("run_directory_setup() creates nested directories", {
  target_dir <-
    file.path(
      tempdir(),
      "PastHumanImpact",
      "nested",
      "dir"
    )

  res_make <-
    run_directory_setup(target_dir)

  testthat::expect_true(dir.exists(target_dir))
  testthat::expect_true(is.logical(res_make))
})

testthat::test_that("run_directory_setup() can be called repeatedly", {
  target_dir <-
    file.path(
      tempdir(),
      "PastHumanImpact",
      "existing_dir"
    )

  res_first <-
    run_directory_setup(target_dir)
  res_second <-
    run_directory_setup(target_dir)

  testthat::expect_true(dir.exists(target_dir))
  testthat::expect_true(is.logical(res_first))
  testthat::expect_true(is.logical(res_second))
})

testthat::test_that("run_directory_setup() validates invalid path input", {
  testthat::expect_error(
    run_directory_setup(dir_path = 1),
    regexp = "single character path"
  )
})