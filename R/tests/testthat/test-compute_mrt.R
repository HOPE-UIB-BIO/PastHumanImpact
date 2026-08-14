testthat::test_that("compute_mrt() validates required columns", {
  bad_input <-
    data.frame(
      dataset_id = 1,
      stringsAsFactors = FALSE
    )

  testthat::expect_error(
    compute_mrt(data_pollen = bad_input),
    regexp = "percentages_harmonised"
  )
})

testthat::test_that("compute_mrt() validates n_rand", {
  data_pollen <-
    data.frame(
      dataset_id = 1,
      percentages_harmonised = I(list(data.frame(sample_id = "s1", a = 1))),
      levels = I(list(data.frame(sample_id = "s1", age = 100))),
      stringsAsFactors = FALSE
    )

  testthat::expect_error(
    compute_mrt(data_pollen = data_pollen, n_rand = -1),
    regexp = "positive"
  )
})
