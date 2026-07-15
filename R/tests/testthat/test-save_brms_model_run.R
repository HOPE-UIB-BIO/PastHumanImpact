testthat::test_that("save_brms_model_run() preserves distinct attempts", {
  model_dir <-
    tempfile(pattern = "model-runs-")
  dir.create(model_dir)
  mod <-
    structure(
      list(value = 1),
      class = "brmsfit"
    )

  first_file <-
    save_brms_model_run(
      mod = mod,
      model_dir = model_dir,
      run_id = "model_a__attempt__1"
    )
  second_file <-
    save_brms_model_run(
      mod = mod,
      model_dir = model_dir,
      run_id = "model_a__attempt__2"
    )

  testthat::expect_true(file.exists(file.path(model_dir, first_file)))
  testthat::expect_true(file.exists(file.path(model_dir, second_file)))
  testthat::expect_length(list.files(model_dir), 2L)
})

testthat::test_that("save_brms_model_run() refuses replacement", {
  model_dir <-
    tempfile(pattern = "model-runs-")
  dir.create(model_dir)
  mod <-
    structure(
      list(value = 1),
      class = "brmsfit"
    )

  save_brms_model_run(
    mod = mod,
    model_dir = model_dir,
    run_id = "model_a__attempt__1"
  )

  testthat::expect_error(
    save_brms_model_run(
      mod = mod,
      model_dir = model_dir,
      run_id = "model_a__attempt__1"
    ),
    regexp = "will not be replaced"
  )
})
