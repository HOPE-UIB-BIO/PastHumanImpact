testthat::test_that("get_pollen_data() selects requested columns and adds percentages", {
  testthat::skip_if_not_installed("REcopol")

  data_assembly <-
    data.frame(
      dataset_id = 1,
      counts_harmonised = I(list(data.frame(
        sample_id = c("s1", "s2"),
        a = c(10, 30),
        b = c(90, 70)
      ))),
      levels = I(list(data.frame(sample_id = c("s1", "s2"), age = c(100, 200)))),
      age_uncertainty = I(list(NULL)),
      pollen_percentage = FALSE,
      end_of_interest_period = 500,
      extra_col = "should_not_appear",
      stringsAsFactors = FALSE
    )

  result <-
    get_pollen_data(data_assembly)

  testthat::expect_true(
    all(c("dataset_id", "counts_harmonised", "levels", "percentages_harmonised") %in% names(result))
  )
  testthat::expect_false("extra_col" %in% names(result))

  pct <-
    purrr::pluck(result, "percentages_harmonised", 1)

  testthat::expect_equal(as.data.frame(pct)[["a"]], c(10, 30))
  testthat::expect_equal(as.data.frame(pct)[["b"]], c(90, 70))
})

testthat::test_that("get_pollen_data() keeps required core columns", {
  testthat::skip_if_not_installed("REcopol")

  data_assembly <-
    data.frame(
      dataset_id = 2,
      counts_harmonised = I(list(data.frame(sample_id = c("x1", "x2"), a = c(1, 2), b = c(3, 4)))),
      levels = I(list(data.frame(sample_id = c("x1", "x2"), age = c(100, 200)))),
      age_uncertainty = I(list(NULL)),
      pollen_percentage = TRUE,
      end_of_interest_period = 1000,
      stringsAsFactors = FALSE
    )

  result <- get_pollen_data(data_assembly)

  testthat::expect_true(
    all(c("dataset_id", "counts_harmonised", "levels", "age_uncertainty", "pollen_percentage", "end_of_interest_period", "percentages_harmonised") %in% names(result))
  )
  testthat::expect_equal(nrow(result), 1L)
})

testthat::test_that("get_pollen_data() handles empty input", {
  testthat::skip_if_not_installed("REcopol")

  data_assembly <-
    data.frame(
      dataset_id = integer(),
      counts_harmonised = I(list()),
      levels = I(list()),
      age_uncertainty = I(list()),
      pollen_percentage = logical(),
      end_of_interest_period = numeric()
    )

  result <- get_pollen_data(data_assembly)

  testthat::expect_equal(nrow(result), 0L)
  testthat::expect_true("percentages_harmonised" %in% names(result))
})

testthat::test_that("get_pollen_data() validates requested variables", {
  testthat::skip_if_not_installed("REcopol")

  data_assembly <-
    data.frame(
      dataset_id = 1,
      counts_harmonised = I(list(data.frame(sample_id = "s1", a = 1))),
      levels = I(list(data.frame(sample_id = "s1", age = 100))),
      age_uncertainty = I(list(NULL)),
      pollen_percentage = FALSE,
      end_of_interest_period = 1000,
      stringsAsFactors = FALSE
    )

  testthat::expect_error(
    get_pollen_data(
      data_assembly = data_assembly,
      variables = c("dataset_id", "missing_column")
    ),
    regexp = "requested `variables`"
  )
})
