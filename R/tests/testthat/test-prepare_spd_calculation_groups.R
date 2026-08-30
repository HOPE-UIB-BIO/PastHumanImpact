testthat::test_that("prepare_spd_calculation_groups() orders large groups first", {
  data_source <-
    tibble::tibble(
      dataset_id = c(2L, 1L),
      curve_name = c("intcal20", "intcal20"),
      rc = list(
        tibble::tibble(value = c(2, 3)),
        tibble::tibble(value = 1)
      )
    )
  data_meta <-
    data_source |>
    dplyr::select(dplyr::all_of(c("dataset_id", "curve_name")))

  result <-
    prepare_spd_calculation_groups(data_source, data_meta)

  testthat::expect_identical(names(result), c("2", "1"))
  testthat::expect_true(all(purrr::map_int(result, nrow) == 1L))
  testthat::expect_identical(result[[1]][["dataset_id"]], 2L)
})

testthat::test_that("prepare_spd_calculation_groups() retains empty datasets", {
  data_source <-
    tibble::tibble(
      dataset_id = 1L,
      curve_name = "intcal20",
      rc = list(tibble::tibble(value = 1))
    )
  data_meta <-
    tibble::tibble(
      dataset_id = 1:2,
      curve_name = c("intcal20", "SHCal20")
    )

  result <-
    prepare_spd_calculation_groups(data_source, data_meta)

  testthat::expect_identical(names(result), c("1", "2"))
  testthat::expect_identical(result[[2]][["curve_name"]], "SHCal20")
  testthat::expect_identical(nrow(result[[2]][["rc"]][[1]]), 0L)
})

testthat::test_that("prepare_spd_calculation_groups() rejects duplicate IDs", {
  data_source <-
    tibble::tibble(
      dataset_id = c(1L, 1L),
      curve_name = c("intcal20", "intcal20"),
      rc = list(tibble::tibble(), tibble::tibble())
    )

  testthat::expect_error(
    prepare_spd_calculation_groups(
      data_source,
      data_source |>
        dplyr::select(dplyr::all_of(c("dataset_id", "curve_name")))
    ),
    regexp = "required contract"
  )
})
