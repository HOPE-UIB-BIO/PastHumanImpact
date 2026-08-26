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
      provenance = list(operation = "mrt")
    )

  testthat::expect_identical(
    validate_mvpart_runtime_result(res_result, operation = "mrt"),
    res_result
  )
})
