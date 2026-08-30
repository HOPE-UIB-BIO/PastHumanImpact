testthat::test_that("diagnose_spd_product_reference() compares products", {
  data_spd <-
    tibble::tibble(
      dataset_id = c("a", "b"),
      spd = list(
        tibble::tibble(age = 0:1, value = c(0, 1)),
        tibble::tibble(age = 0:1, value = c(1, 2))
      ),
      distance = c(250, 500)
    )
  data_reference <-
    tibble::tibble(
      dataset_id = c("a", "c"),
      spd = list(
        tibble::tibble(age = 0:1, value = c(0, 1)),
        tibble::tibble(age = 0:1, value = c(2, 3))
      ),
      distance = c(250, 500)
    )

  result <-
    diagnose_spd_product_reference(data_spd, data_reference)

  testthat::expect_equal(nrow(result), 3L)
  testthat::expect_true(
    dplyr::filter(result, dataset_id == "a")[["within_tolerance"]]
  )
  testthat::expect_false(
    dplyr::filter(result, dataset_id == "b")[["present_reference"]]
  )
  testthat::expect_false(
    dplyr::filter(result, dataset_id == "c")[["present_new"]]
  )
})

testthat::test_that("diagnose_spd_product_reference() detects changes", {
  data_spd <-
    tibble::tibble(
      dataset_id = "a",
      spd = list(tibble::tibble(age = 0:1, value = c(0, 1))),
      distance = 250
    )
  data_reference <-
    tibble::tibble(
      dataset_id = "a",
      spd = list(tibble::tibble(age = 0:1, value = c(0, 2))),
      distance = 500
    )

  result <-
    diagnose_spd_product_reference(data_spd, data_reference)

  testthat::expect_false(result[["identical_distance"]])
  testthat::expect_equal(result[["max_absolute_difference"]], 1)
  testthat::expect_false(result[["within_tolerance"]])
})

testthat::test_that("diagnose_spd_product_reference() validates inputs", {
  data_spd <-
    tibble::tibble(
      dataset_id = "a",
      spd = list(tibble::tibble(age = 0, value = 1))
    )

  testthat::expect_error(
    diagnose_spd_product_reference(data_spd, data_spd),
    regexp = "do not satisfy the contract"
  )
})
