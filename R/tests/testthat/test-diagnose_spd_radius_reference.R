testthat::test_that("diagnose_spd_radius_reference() detects exact agreement", {
  data_spd <-
    tibble::tibble(
      dataset_id = c(1L, 1L),
      radius_km = c(250L, 500L),
      spd = list(
        tibble::tibble(age = 0:1, value = c(0.1, 0.2)),
        tibble::tibble(age = 0:1, value = c(9, 9))
      )
    )

  data_reference <-
    tibble::tibble(
      dataset_id = 1L,
      spd = list(tibble::tibble(age = 0:1, `250` = c(0.1, 0.2)))
    )

  result <-
    diagnose_spd_radius_reference(
      data_spd = data_spd,
      data_reference = data_reference,
      radius_km = 250L
    )

  testthat::expect_true(result[["within_tolerance"]])
  testthat::expect_identical(result[["max_absolute_difference"]], 0)
  testthat::expect_equal(nrow(result), 1L)
})

testthat::test_that("diagnose_spd_radius_reference() detects differences", {
  data_spd <-
    tibble::tibble(
      dataset_id = 1L,
      radius_km = 250L,
      spd = list(tibble::tibble(age = 0:1, value = c(0.1, 0.3)))
    )

  data_reference <-
    tibble::tibble(
      dataset_id = 1L,
      spd = list(tibble::tibble(age = 0:1, `250` = c(0.1, 0.2)))
    )

  result <-
    diagnose_spd_radius_reference(
      data_spd = data_spd,
      data_reference = data_reference,
      radius_km = 250L,
      tolerance = 0.01
    )

  testthat::expect_false(result[["within_tolerance"]])
  testthat::expect_equal(result[["max_absolute_difference"]], 0.1)
})

testthat::test_that(
  "diagnose_spd_radius_reference() retains unmatched cohort rows",
  {
    reference_spd <- tibble::tibble(age = 0:1)
    reference_spd[["250"]] <- c(0.1, 0.2)
    data_spd <-
      tibble::tibble(
        dataset_id = 1L,
        radius_km = 250L,
        spd = list(tibble::tibble(age = 0:1, value = c(0.1, 0.2)))
      )
    data_reference <-
      tibble::tibble(
        dataset_id = 2L,
        spd = list(reference_spd)
      )

    result <-
      diagnose_spd_radius_reference(
        data_spd = data_spd,
        data_reference = data_reference,
        radius_km = 250L
      )

    testthat::expect_equal(nrow(result), 2L)
    testthat::expect_true(
      result[["present_new"]][result[["dataset_id"]] == 1L]
    )
    testthat::expect_false(
      result[["present_reference"]][result[["dataset_id"]] == 1L]
    )
    testthat::expect_false(
      result[["within_tolerance"]][result[["dataset_id"]] == 1L]
    )
    testthat::expect_false(
      result[["present_new"]][result[["dataset_id"]] == 2L]
    )
    testthat::expect_true(
      result[["present_reference"]][result[["dataset_id"]] == 2L]
    )
  }
)
