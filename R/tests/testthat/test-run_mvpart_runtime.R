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

testthat::test_that("run_mvpart_runtime() validates seed", {
  path_runner <-
    base::tempfile(fileext = ".R")

  path_installer <-
    base::tempfile(fileext = ".R")

  path_function <-
    base::tempfile(fileext = ".R")

  path_library <-
    base::tempfile()

  base::file.create(path_runner, path_installer, path_function)
  base::dir.create(path_library)

  testthat::expect_error(
    run_mvpart_runtime(
      operation = "mrt",
      data_input = data.frame(dataset_id = 1),
      path_runner = path_runner,
      path_installer = path_installer,
      path_function_files = path_function,
      seed = 1.5,
      path_library = path_library,
      rig_executable = "rig"
    ),
    regexp = "arguments are invalid"
  )
})

testthat::test_that("run_mvpart_runtime() passes seed to the runtime", {
  path_runner <-
    base::tempfile(fileext = ".R")

  path_installer <-
    base::tempfile(fileext = ".R")

  path_function <-
    base::tempfile(fileext = ".R")

  path_library <-
    base::tempfile()

  base::file.create(path_runner, path_installer, path_function)
  base::dir.create(path_library)

  process_runner <-
    function(command, args, ...) {
      res_request <-
        base::readRDS(args[[7]])

      data_result <-
        data.frame(dataset_id = 1, mvrt_groups_n = 1)

      data_result[["mvrt_partitions"]] <-
        I(list(data.frame(sample_id = "a", MRT_partitions = 1)))

      data_result[["mvrt_cp"]] <-
        I(list(numeric(0)))

      base::saveRDS(
        object = list(
          data = data_result,
          provenance = list(
            operation = res_request[["operation"]],
            seed = res_request[["seed"]]
          )
        ),
        file = args[[8]]
      )

      res <-
        list(status = 0L, stdout = "", stderr = "")

      return(res)
    }

  res <-
    run_mvpart_runtime(
      operation = "mrt",
      data_input = data.frame(dataset_id = 1),
      path_runner = path_runner,
      path_installer = path_installer,
      path_function_files = path_function,
      seed = 900723L,
      path_library = path_library,
      rig_executable = "rig",
      process_runner = process_runner
    )

  testthat::expect_identical(res[["provenance"]][["seed"]], 900723L)
})

testthat::test_that("run_mvpart_runtime() rejects a result without seed", {
  path_runner <-
    base::tempfile(fileext = ".R")

  path_installer <-
    base::tempfile(fileext = ".R")

  path_function <-
    base::tempfile(fileext = ".R")

  path_library <-
    base::tempfile()

  base::file.create(path_runner, path_installer, path_function)
  base::dir.create(path_library)

  process_runner <-
    function(command, args, ...) {
      data_result <-
        data.frame(dataset_id = 1, mvrt_groups_n = 1)

      data_result[["mvrt_partitions"]] <-
        I(list(data.frame(sample_id = "a", MRT_partitions = 1)))

      data_result[["mvrt_cp"]] <-
        I(list(numeric(0)))

      base::saveRDS(
        object = list(
          data = data_result,
          provenance = list(operation = "mrt")
        ),
        file = args[[8]]
      )

      res <-
        list(status = 0L, stdout = "", stderr = "")

      return(res)
    }

  testthat::expect_error(
    run_mvpart_runtime(
      operation = "mrt",
      data_input = data.frame(dataset_id = 1),
      path_runner = path_runner,
      path_installer = path_installer,
      path_function_files = path_function,
      seed = 900723L,
      path_library = path_library,
      rig_executable = "rig",
      process_runner = process_runner
    ),
    regexp = "requested seed"
  )
})
