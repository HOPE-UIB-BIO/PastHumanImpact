testthat::test_that("run_mvpart_runtime() validates isolated library", {
  path_runner <-
    tempfile(fileext = ".R")

  path_installer <-
    tempfile(fileext = ".R")

  path_function <-
    tempfile(fileext = ".R")

  file.create(path_runner, path_installer, path_function)

  testthat::expect_error(
    run_mvpart_runtime(
      operation = "mrt",
      data_input = data.frame(dataset_id = 1),
      path_runner = path_runner,
      path_installer = path_installer,
      path_function_files = path_function,
      path_library = tempfile()
    ),
    regexp = "arguments are invalid"
  )
})
