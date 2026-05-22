testthat::test_that("get_diversity() adds PAP_diversity list-column", {
  testthat::skip_if_not_installed("REcopol")

  data_pollen <- tibble::tibble(
    dataset_id = c(1L, 2L),
    counts_harmonised = list(
      data.frame(
        sample_id = c("s1", "s2"),
        TaxaA = c(10L, 5L),
        TaxaB = c(5L, 20L),
        stringsAsFactors = FALSE
      ),
      data.frame(
        sample_id = c("s3", "s4"),
        TaxaA = c(8L, 2L),
        TaxaB = c(2L, 8L),
        TaxaC = c(1L, 1L),
        stringsAsFactors = FALSE
      )
    )
  )

  res_div <-
    get_diversity(data_pollen = data_pollen, n_rand = 9)

  testthat::expect_equal(names(res_div), c("dataset_id", "PAP_diversity"))
  testthat::expect_equal(nrow(res_div), 2L)
})

testthat::test_that("get_diversity() keeps dataset_id ordering", {
  testthat::skip_if_not_installed("REcopol")

  data_pollen <- tibble::tibble(
    dataset_id = c(11L, 5L),
    counts_harmonised = list(
      data.frame(sample_id = "s1", TaxaA = 10L, TaxaB = 5L),
      data.frame(sample_id = "s2", TaxaA = 8L, TaxaB = 3L)
    )
  )

  res_div <-
    get_diversity(data_pollen = data_pollen, n_rand = 7)

  testthat::expect_identical(
    dplyr::pull(res_div, dataset_id),
    c(11L, 5L)
  )
})

testthat::test_that("get_diversity() handles empty input", {
  testthat::skip_if_not_installed("REcopol")

  data_pollen <-
    tibble::tibble(
      dataset_id = integer(),
      counts_harmonised = list()
    )

  res_div <-
    get_diversity(data_pollen = data_pollen, n_rand = 5)

  testthat::expect_equal(names(res_div), c("dataset_id", "PAP_diversity"))
  testthat::expect_equal(nrow(res_div), 0L)
})

testthat::test_that("get_diversity() validates required columns", {
  bad_input <-
    tibble::tibble(
      dataset_id = 1L
    )

  testthat::expect_error(
    get_diversity(data_pollen = bad_input),
    regexp = "counts_harmonised"
  )
})
