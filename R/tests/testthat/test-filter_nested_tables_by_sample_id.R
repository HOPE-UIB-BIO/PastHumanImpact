testthat::test_that("filter_nested_tables_by_sample_id() filters each table", {
  data_source <-
    data.frame(dataset_id = c(1, 2))

  data_source[["levels"]] <-
    I(
      list(
        data.frame(sample_id = c("a", "b"), age = c(0, 1)),
        data.frame(sample_id = c("c", "d"), age = c(2, 3))
      )
    )

  data_source[["valid_ids"]] <-
    I(list("b", "c"))

  data_result <-
    filter_nested_tables_by_sample_id(
      data_source = data_source,
      table_name = "levels",
      id_name = "valid_ids"
    )

  testthat::expect_identical(
    purrr::map(data_result[["levels"]], ~ .x[["sample_id"]]),
    list("b", "c")
  )
})
