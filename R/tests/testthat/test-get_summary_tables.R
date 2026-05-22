testthat::test_that("get_summary_tables errors if group_var column is missing", {
  data_bad <- tibble::tibble(
    age       = c(100, 200),
    varhp     = list(NULL, NULL),
    data_merge = list(NULL, NULL)
  )
  testthat::expect_error(
    get_summary_tables(data_bad, data_type = "temporal", group_var = "region"),
    regexp = "region"
  )
})

testthat::test_that("get_summary_tables temporal returns list with summary_table and summary_table_weighted_mean", {
  testthat::skip_if_not_installed("janitor")
  testthat::skip_if_not_installed("assertthat")

  st <- data.frame(
    predictor  = c("temp", "prec"),
    Unique     = c(0.2, 0.1),
    Individual = c(0.3, 0.15),
    stringsAsFactors = FALSE
  )

  data_source <- tibble::tibble(
    region     = c("EU", "EU"),
    age        = c(100L, 200L),
    data_merge = list(NULL, NULL),
    varhp      = list(
      list(summary_table = st),
      list(summary_table = st)
    )
  )

  result <- get_summary_tables(
    data_source,
    data_type  = "temporal",
    group_var  = "region"
  )

  testthat::expect_type(result, "list")
  testthat::expect_true("summary_table" %in% names(result))
  testthat::expect_true("summary_table_weighted_mean" %in% names(result))

  testthat::expect_true(nrow(result$summary_table) > 0)
  testthat::expect_true(nrow(result$summary_table_weighted_mean) > 0)
})

testthat::test_that("get_summary_tables validates required varhp/data_merge columns", {
  data_bad <-
    data.frame(
      region = "EU",
      age = 100,
      stringsAsFactors = FALSE
    )

  testthat::expect_error(
    get_summary_tables(
      data_source = data_bad,
      data_type = "temporal",
      group_var = "region"
    ),
    regexp = "varhp"
  )
})
