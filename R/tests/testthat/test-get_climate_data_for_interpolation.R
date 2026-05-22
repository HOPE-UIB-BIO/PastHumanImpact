testthat::test_that("get_climate_data_for_interpolation() unnests and nests by selected vars", {
  climate_df <- data.frame(
    age         = c(100, 200),
    time_id     = c(1L, 2L),
    temp_annual = c(10.0, 11.0),
    temp_cold   = c(-5.0, -4.0),
    gdm         = c(3L, 4L),
    stringsAsFactors = FALSE
  )

  data_source <- tibble::tibble(
    dataset_id   = 1L,
    climate_data = list(climate_df)
  )

  result <- get_climate_data_for_interpolation(
    data_source,
    sel_var = c("temp_annual", "temp_cold")
  )

  testthat::expect_equal(nrow(result), 2L)
  testthat::expect_true(all(c("var_name", "data_to_fit") %in% names(result)))
  testthat::expect_equal(sort(dplyr::pull(result, var_name)), c("temp_annual", "temp_cold"))
  testthat::expect_false("gdm" %in% dplyr::pull(result, var_name))

  df1 <- dplyr::pull(result, data_to_fit)[[1]]
  testthat::expect_equal(nrow(df1), 2L)
  testthat::expect_false("time_id" %in% names(df1))
})

testthat::test_that("get_climate_data_for_interpolation() keeps expected rows per variable", {
  climate_df <- data.frame(
    age = c(100, 200, 300),
    time_id = c(1L, 2L, 3L),
    temp_annual = c(10, 11, 12),
    temp_cold = c(-5, -4, -3),
    prec_annual = c(100, 120, 140),
    prec_summer = c(50, 60, 70),
    prec_win = c(30, 35, 40),
    gdm = c(2L, 3L, 4L)
  )

  data_source <- tibble::tibble(dataset_id = 1L, climate_data = list(climate_df))

  result <-
    get_climate_data_for_interpolation(
      data_source = data_source,
      sel_var = c("temp_annual", "prec_annual")
    )

  nested_a <-
    dplyr::filter(result, var_name == "temp_annual") |>
    dplyr::pull(data_to_fit) |>
    .subset2(1L)

  nested_b <-
    dplyr::filter(result, var_name == "prec_annual") |>
    dplyr::pull(data_to_fit) |>
    .subset2(1L)

  testthat::expect_equal(nrow(nested_a), 3L)
  testthat::expect_equal(nrow(nested_b), 3L)
})

testthat::test_that("get_climate_data_for_interpolation() returns empty for unmatched sel_var", {
  climate_df <- data.frame(
    age = c(100, 200),
    time_id = c(1L, 2L),
    temp_annual = c(10, 11),
    temp_cold = c(-5, -4),
    prec_annual = c(100, 120),
    prec_summer = c(50, 60),
    prec_win = c(30, 35),
    gdm = c(2L, 3L)
  )

  data_source <- tibble::tibble(dataset_id = 1L, climate_data = list(climate_df))

  result <-
    get_climate_data_for_interpolation(
      data_source = data_source,
      sel_var = c("does_not_exist")
    )

  testthat::expect_equal(nrow(result), 0L)
})

testthat::test_that("get_climate_data_for_interpolation() validates climate_data column", {
  bad_source <-
    data.frame(
      dataset_id = 1L,
      stringsAsFactors = FALSE
    )

  testthat::expect_error(
    get_climate_data_for_interpolation(data_source = bad_source),
    regexp = "climate_data"
  )
})
