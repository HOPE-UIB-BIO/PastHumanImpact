testthat::test_that("load_brms_model_file() loads the exact file", {
  model_dir <-
    tempfile(pattern = "model-runs-")
  dir.create(model_dir)
  mod <-
    structure(
      list(value = 1),
      class = "brmsfit"
    )
  model_file_name <-
    save_brms_model_run(
      mod = mod,
      model_dir = model_dir,
      run_id = "model_a__attempt__1"
    )

  result <-
    load_brms_model_file(
      model_dir = model_dir,
      model_file_name = model_file_name
    )

  testthat::expect_s3_class(result, "brmsfit")
  testthat::expect_identical(result[["value"]], 1)
})

testthat::test_that("load_brms_model_file() rejects missing exact files", {
  model_dir <-
    tempfile(pattern = "model-runs-")
  dir.create(model_dir)

  testthat::expect_error(
    load_brms_model_file(
      model_dir = model_dir,
      model_file_name = "missing.qs"
    ),
    regexp = "does not exist"
  )
})
