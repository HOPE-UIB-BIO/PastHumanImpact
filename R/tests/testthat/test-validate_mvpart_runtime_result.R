testthat::test_that("validate_mvpart_runtime_result() accepts MRT output", {
  data_result <-
    data.frame(
      dataset_id = 1,
      mvrt_groups_n = 2
    )

  data_result[["mvrt_partitions"]] <-
    I(list(data.frame(sample_id = "a", MRT_partitions = 1)))

  data_result[["mvrt_cp"]] <-
    I(list(numeric(0)))

  res_result <-
    list(
      data = data_result,
      provenance = list(operation = "mrt", seed = 1234L)
    )

  testthat::expect_identical(
    validate_mvpart_runtime_result(
      res_result,
      operation = "mrt",
      seed = 1234L
    ),
    res_result
  )
})

testthat::test_that("validate_mvpart_runtime_result() validates seed", {
  data_result <-
    data.frame(
      dataset_id = 1,
      mvrt_groups_n = 2
    )

  data_result[["mvrt_partitions"]] <-
    I(list(data.frame(sample_id = "a", MRT_partitions = 1)))

  data_result[["mvrt_cp"]] <-
    I(list(numeric(0)))

  res_result <-
    list(
      data = data_result,
      provenance = list(operation = "mrt", seed = 1234L)
    )

  testthat::expect_error(
    validate_mvpart_runtime_result(
      res_result,
      operation = "mrt",
      seed = 1.5
    ),
    regexp = "result contract is invalid"
  )
})

testthat::test_that("validate_mvpart_runtime_result() rejects other seeds", {
  data_result <-
    data.frame(
      dataset_id = 1,
      mvrt_groups_n = 2
    )

  data_result[["mvrt_partitions"]] <-
    I(list(data.frame(sample_id = "a", MRT_partitions = 1)))

  data_result[["mvrt_cp"]] <-
    I(list(numeric(0)))

  res_other_seed <-
    list(
      data = data_result,
      provenance = list(operation = "mrt", seed = 1L)
    )

  res_no_seed <-
    list(
      data = data_result,
      provenance = list(operation = "mrt")
    )

  testthat::expect_error(
    validate_mvpart_runtime_result(
      res_other_seed,
      operation = "mrt",
      seed = 1234L
    ),
    regexp = "requested seed"
  )

  testthat::expect_error(
    validate_mvpart_runtime_result(
      res_no_seed,
      operation = "mrt",
      seed = 1234L
    ),
    regexp = "requested seed"
  )
})
