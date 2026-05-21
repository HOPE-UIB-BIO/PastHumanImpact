testthat::test_that("get_dcca adds dcca_scores and dcca_grad_length, filters datasets with failed ordinations", {
  testthat::skip_if_not_installed("REcopol")

  pct <- data.frame(
    sample_id = c("s1", "s2", "s3", "s4", "s5"),
    TaxaA = c(60, 40, 20, 10, 5),
    TaxaB = c(30, 40, 50, 60, 70),
    TaxaC = c(10, 20, 30, 30, 25),
    stringsAsFactors = FALSE
  )
  lvl <- data.frame(
    sample_id = c("s1", "s2", "s3", "s4", "s5"),
    age       = c(100, 200, 300, 400, 500),
    stringsAsFactors = FALSE
  )

  data_pollen <- tibble::tibble(
    dataset_id            = 1L,
    percentages_harmonised = list(pct),
    levels                 = list(lvl)
  )

  result <- get_dcca(data_pollen)

  testthat::expect_true(
    all(c("dataset_id", "dcca", "dcca_scores", "dcca_grad_length") %in% names(result))
  )
  testthat::expect_equal(nrow(result), 1L)

  scores <- dplyr::pull(result, dcca_scores)[[1]]
  testthat::expect_true(is.data.frame(scores) || tibble::is_tibble(scores))
  testthat::expect_true("sample_id" %in% names(scores))
  testthat::expect_true("axis_1" %in% names(scores))

  testthat::expect_true(is.numeric(dplyr::pull(result, dcca_grad_length)[[1]]))
  testthat::expect_gt(dplyr::pull(result, dcca_grad_length)[[1]], 0)
})

testthat::test_that("get_dcca() returns expected columns and classes", {
  testthat::skip_if_not_installed("REcopol")

  pct <- data.frame(
    sample_id = c("s1", "s2", "s3", "s4", "s5"),
    TaxaA = c(50, 40, 30, 20, 10),
    TaxaB = c(20, 25, 30, 35, 40),
    TaxaC = c(30, 35, 40, 45, 50),
    stringsAsFactors = FALSE
  )

  lvl <- data.frame(
    sample_id = c("s1", "s2", "s3", "s4", "s5"),
    age = c(100, 200, 300, 400, 500),
    stringsAsFactors = FALSE
  )

  data_pollen <-
    tibble::tibble(
      dataset_id = 10L,
      percentages_harmonised = list(pct),
      levels = list(lvl)
    )

  result <- get_dcca(data_pollen)

  testthat::expect_true(all(c("dataset_id", "dcca", "dcca_scores", "dcca_grad_length") %in% names(result)))
  testthat::expect_true(is.list(dplyr::pull(result, dcca)))
  testthat::expect_true(is.list(dplyr::pull(result, dcca_scores)))
})

testthat::test_that("get_dcca() handles empty input", {
  testthat::skip_if_not_installed("REcopol")

  data_pollen <-
    tibble::tibble(
      dataset_id = integer(),
      percentages_harmonised = list(),
      levels = list()
    )

  result <- get_dcca(data_pollen)

  testthat::expect_equal(nrow(result), 0L)
  testthat::expect_true(all(c("dataset_id", "dcca", "dcca_scores", "dcca_grad_length") %in% names(result)))
})
